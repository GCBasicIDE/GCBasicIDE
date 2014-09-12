'	GCBASIC - A BASIC Compiler for microcontrollers
'	Copyright (C) 2006 - 2013 Hugh Considine
'
'	This program is free software; you can redistribute it and/or modify
'	it under the terms of the GNU General Public License as published by
'	the Free Software Foundation; either version 2 of the License, or
'	(at your option) any later version.
'
'	This program is distributed in the hope that it will be useful,
'	but WITHOUT ANY WARRANTY; without even the implied warranty of
'	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'	GNU General Public License for more details.
'
'	You should have received a copy of the GNU General Public License
'	along with this program; if not, write to the Free Software
'	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'
'If you have any questions about the source code, please email me: hconsidine@internode.on.net
'Any other questions, please email me or see the GCBASIC forums.

'Array sizes
#Define MAX_PROG_PAGES 20

'Type for sections of code returned from subs
Type LinkedListElement
	Value As String
	NumVal As Integer
	MetaData As Any Pointer
	Prev As LinkedListElement Pointer
	Next As LinkedListElement Pointer
End Type

'Meta data for program line
Type ProgLineMeta
	RequiredBank As Integer 'Bank chip needs for this icon, -1 if N/A
	VarInBank As String 'Variable in the bank that is to be changed to
	
	IsAutoPageSel As Integer 'Set to -1 if line is automatically added pagsel
	
	'List of commands that lead to this one
	PrevCommands As LinkedListElement Pointer
	'List of commands that this one leads to
	NextCommands As LinkedListElement Pointer
	
	AsmCommand As Integer 'Index of asm instruction on line. -1 if none.
	
End Type

Type VariableType
	Name As String
	Type As String
	Size As Integer
	Pointer As String
	Alias As String
	Origin As String
	
	FixedLocation As Integer
	Location As Integer
	IsSharedBank As Integer
	
	FixedSize As Integer
	
End Type

Type VariableListElement
	Name As String
	Value As String
	IsArray As Integer
End Type

Type IntData
	Vector As String
	VectorLoc As Integer
	EventName As String
	EnableBit As String
	FlagBit As String
	Handler As String
End Type

Type SubParam
	Name As String
	Type As String
	Dir As Integer '1 - in, 2 - out, 3 - in/out
	Default As String
End Type

Type SubType
	Name As String
	SourceFile As Integer
	IsMacro As Integer
	IsFunction As Integer
	ReturnType As String
	Params(50) As SubParam
	ParamCount As Integer
	Overloaded As Integer
	
	'Variables in subroutine
	Variable(2000) As VariableType
	Variables As Integer
	VarsRead As Integer
	
	'Flags
	Required As Integer
	Compiled As Integer
	NoReturn As Integer
	HasFinalGoto As Integer
	
	'If ends in goto, record sub jumped to
	FinalGotoDest As String
	
	'Call tree
	CallList As LinkedListElement Pointer
	
	'Original BASIC size (for compilation report)
	OriginalLOC As Integer
	
	'Program memory page selection hints
	HexSize As Integer
	DestPage As Integer
	FirstPage As Integer
	LocationSet As Integer
	MaxHexSize As Integer 'Highest recorded size
	CallsFromPage(MAX_PROG_PAGES) As Integer 'Record calls from each page
	
	'Sub code
	CodeStart As LinkedListElement Pointer
	
End Type

Type DataTableType
	Name As String
	Type As String
	Origin As String
	StoreLoc As Integer '0 = Flash, 1 = Data EEPROM
	Used As Integer
	Items As Integer
	Item(255) As Integer
End Type

Type ProgString
	Value As String
	Used As Integer
End Type

Type SubCallType
	Called As SubType Pointer
	Caller As SubType Pointer
	CalledID As Integer
	Origin As String
	Param(50, 2) As String
	Params As Integer
	CallSig As String
End Type

Type SysVarType
	Name As String
	Location As Integer
	Parent As String
End Type

Type AsmCommand
	Syntax As String
	Words As Integer
	Word(4) As String
	
	Cmd As String
	Params As Integer
	Param(25) As String
End Type

Type ConfigOp
	Op As String
	Loc As Integer
	Val As Integer
End Type

Type ConfigSetting
	Name As String 'Name of config setting
	Setting As LinkedListElement Pointer 'Points to current option
	Options As LinkedListElement Pointer 'List of potential settings
End Type

Type CalcVar
	MaxType As String
	Status As String
	CurrentType As String
	CurrentValue As String
	High As Integer
End Type

Type ExternalTool
	Name As String
	Type As String
	Cmd As String
	Params As String
	WorkingDir As String
	ExtraParam(5, 2) As String
	ExtraParams As Integer
End Type

Type FileConverterType
	Name As String
	Desc As String
	InFormat(10) As String
	InFormats As Integer
	OutFormat As String
	ExeName As String
End Type

'Type to store generated code
Type CodeSection
	CodeList As LinkedListElement Pointer
	CodeEnd As LinkedListElement Pointer
End Type

'Type to store a program memory page
Type ProgMemPageType
	StartLoc As Integer
	EndLoc As Integer
	CodeSize As Integer
	MaxSize As Integer
End Type

'Type to store a RAM bank
Type DataMemBankType
	StartLoc As Integer 'Start address of bank
	EndLoc As Integer 'End address of bank
	DataSize As Integer 'Amount of bank currently used
End Type

'Type to store info on which page is best
Type AllocationOrderType
	Page As Integer
	Calls As Integer
End Type

'Type to store usage info on an I/O pin
Type PinDirType
	'Name of the pin stored in LinkedListElement
	'This type used as metadata
	'Record pin reads and writes
	WrittenTo As Integer
	ReadFrom As Integer
	'Set to -1 if whole port set, but direction may not be known 
	WholePort As Integer
	'Record pin direction settings
	SetOut As Integer
	SetIn As Integer
	'Allowed directions (from chip data)
	AllowedDirections As String
	'If Dir command sets entire port to constant, store here
	'Set to -1 if set by a variable
	WholePortDir As Integer
End Type

'Subs in this file
DECLARE SUB Add18FBanks(CompSub As SubType Pointer)
DECLARE SUB AddBankCommands(CompSub As SubType Pointer)
Declare Sub AddMainEndCode
Declare Sub AddMainInitCode
Declare Sub AddPageCommands(CompSub As SubType Pointer)
Declare Sub AddInterruptCode
DECLARE SUB AddSysVarBits (CompSub As SubType Pointer)
Declare SUB BuildMemoryMap
DECLARE SUB CalcConfig
DECLARE Sub CalcOps (OutList As CodeSection Pointer, SUM As String, AV As String, Ops As String, OriginIn As String)
Declare Function CalcLineSize(CurrLine As String, ThisSubPage As Integer, CallPos As Integer = -1, GotoPos As Integer = -1, NextCodeLine As String = "") As Integer
Declare Sub CalcSubSize(CurrSub As SubType Pointer)
DECLARE FUNCTION CastOrder (InType As String) As Integer
Declare Sub CheckConstName (ConstName As String, Origin As String)
Declare Sub CheckClockSpeed
Declare Sub CompileProgram
Declare Sub CompileSubroutine(CompSub As SubType Pointer)
Declare Sub CompileCalc (SUM As String, AV As String, Origin As String, ByRef OutList As CodeSection Pointer = 0)
DECLARE FUNCTION CompileCalcAdd(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
DECLARE FUNCTION CompileCalcCondition(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
DECLARE FUNCTION CompileCalcLogic(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
DECLARE FUNCTION CompileCalcMult(CodeSection As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
Declare Function CompileCalcUnary(CodeSection As CodeSection Pointer, Act As String, V2 As String, Origin As String, AnswerIn As String) As String
Declare Function CompileConditions (Condition As String, IfTrue As String, Origin As String) As LinkedListElement Pointer
DECLARE Sub CompileDim (CurrSub As SubType Pointer)
DECLARE SUB CompileDir (CompSub As SubType Pointer)
DECLARE SUB CompileDo (CompSub As SubType Pointer)
DECLARE SUB CompileExitSub (CompSub As SubType Pointer)
DECLARE SUB CompileFor (CompSub As SubType Pointer)
Declare Sub CompileGoto (CompSub As SubType Pointer)
DECLARE SUB CompileIF (CompSub As SubType Pointer)
Declare Sub CompileIntOnOff (CompSub As SubType Pointer)
Declare SUB CompileOn (CompSub As SubType Pointer)
DECLARE SUB CompilePot (CompSub As SubType Pointer)
DECLARE SUB CompileReadTable (CompSub As SubType Pointer)
DECLARE SUB CompileRepeat (CompSub As SubType Pointer)
DECLARE SUB CompileRotate (CompSub As SubType Pointer)
DECLARE SUB CompileSelect (CompSub As SubType Pointer)
DECLARE SUB CompileSet (CompSub As SubType Pointer)
Declare Function CompileString (InLine As String, Origin As String) As LinkedListElement Pointer
Declare Function CompileSubCall (InCall As SubCallType Pointer) As LinkedListElement Pointer
Declare Sub CompileSubCalls (CompSub As SubType Pointer)
DECLARE SUB CompileTables
DECLARE SUB CompileVars (CompSub As SubType Pointer)
Declare Function CompileVarSet (SourceIn As String, Dest As String, Origin As String) As LinkedListElement Pointer
DECLARE SUB CompileWait (CompSub As SubType Pointer)
Declare Function CompileWholeArray (InLine As String, Origin As String) As LinkedListElement Pointer
Declare Function ConfigNameMatch(ConfigIn As String, ConfigNameIn As String) As Integer
Declare Function ConfigValueMatch(ConfigIn As String, ConfigValueIn As String) As Integer
Declare Sub CreateCallTree
Declare Sub DisplayProgram
Declare Sub DisplayCallTree
Declare Sub ExtAssembler
Declare Sub ExtractParameters(ByRef NewSubCall As SubCallType, InLineCopy As String, Origin As String)
Declare Sub FinalOptimise
Declare Sub FindAssembly (CompSub As SubType Pointer)
Declare Function FindPotentialBanks(CurrLine As LinkedListElement Pointer, OutList As LinkedListElement Pointer = 0, CheckedLines As LinkedListElement Pointer = 0) As LinkedListElement Pointer
Declare Sub FindUncompiledLines
Declare Function FixBit (InBit As String, Origin As String) As String
DECLARE SUB FixFunctions (CompSub As SubType Pointer)
Declare Sub FixPointerOps (CompSub As SubType Pointer)
Declare Sub FixSinglePinSet
Declare Sub FreeCalcVar (VarName As String)
Declare Function GenerateArrayPointerSet(DestVar As String, DestPtr As Integer, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer
Declare Function GenerateAutoPinDir As LinkedListElement Pointer
Declare Function GenerateBitSet(BitNameIn As String, NewStatus As String, Origin As String) As LinkedListElement Pointer
Declare Function GenerateExactDelay(ByVal Cycles As Integer) As LinkedListElement Pointer
Declare Function GenerateVectorCode As LinkedListElement Pointer
Declare Function GetCalcType(VT1 As String, Act As String, VT2 As String) As String
Declare Function GetCalcVar (VarTypeIn As String) As String
Declare Function GetCalledSubs(CurrSub As SubType Pointer, ExistingList As LinkedListElement Pointer = 0) As LinkedListElement Pointer
DECLARE FUNCTION GetDestSub(Origin As String) As Integer
Declare Sub GetEqConfig
Declare Function GetLabelList(CompSub As SubType Pointer) As LinkedListElement Pointer
Declare Function GetMetaData(CurrLine As LinkedListElement Pointer) As ProgLineMeta Pointer
Declare Function GetPinDirection(PinNameIn As String) As PinDirType Pointer
Declare Function GetRealIOName(InName As String) As String
Declare Function GetRegisterLoc(RegName As String) As Integer
DECLARE FUNCTION GetSub(Origin As String) As String
Declare Function GetSubFullName(SubIndex As Integer) As String
Declare FUNCTION GetSubID(Origin As String) As Integer
Declare Function GetSubSig(CurrentSub As SubType) As String
Declare Function GetSubParam (ParamIn As String, ForceIn As Integer) As SubParam
DECLARE SUB InitCompiler
DECLARE FUNCTION IsArray (VarName As String, CurrSub As SubType Pointer) As Integer
Declare Function IsNonBanked(Location As Integer) As Integer
Declare Function IsInAccessBank(VarNameIn As String) As Integer
Declare Function IsIOPinName(PinName As String) As Integer
Declare Function IsIOReg (RegNameIn As String) As Integer
Declare Function IsLowIOReg (RegNameIn As String) As Integer
Declare Function IsLowRegister(VarName As String) As Integer
Declare Function IsRegister (VarName As String) As Integer
Declare FUNCTION IsString (InData As String, CurrSub As SubType Pointer) As Integer
Declare Function IsUnaryOp (InData As String) As Integer
DECLARE FUNCTION IsWord (InData As String, CurrentSub As Integer) As Integer
Declare Sub LoadConverters
DECLARE FUNCTION LocationOfSub (SubNameIn As String, SubSigIn As String, Origin As String = "", AllowVague As Integer = 0) As Integer
Declare Sub LogError(InMessage As String, Origin As String = "")
Declare Sub LogOutputMessage(InMessage As String)
Declare Sub LogWarning(InMessage As String, Origin As String = "")
Declare Sub MergeSubroutines
DECLARE FUNCTION Message (InData As String) As String
DECLARE Function ModeAVR As Integer
DECLARE Function ModePIC As Integer
Declare Function ModeZ8 As Integer
Declare Function NewCodeSection As CodeSection Pointer
Declare Function NewProgLineMeta As ProgLineMeta Pointer
Declare Function NewSubroutine(SubName As String) As SubType Pointer
Declare Sub OptimiseCalls
DECLARE SUB OptimiseIF(CompSub As SubType Pointer = 0)
Declare Sub PreparePageData
DECLARE SUB ProcessArrays (CompSub As SubType Pointer)
DECLARE SUB ProcessWords ()
Declare Function PutInRegister(ByRef OutList As LinkedListElement Pointer, SourceValue As String, RegType As String, Origin As String) As String
DECLARE SUB ReadChipData
Declare Sub ReadOptions(OptionsIn As String)
Declare Sub RecordSubCall(CompSub As SubType Pointer, CalledSub As SubType Pointer)
Declare Function ReplaceFnNames(InName As String) As String
Declare Function RequestSub(Requester As SubType Pointer, SubNameIn As String, SubSigIn As String = "") As Integer
Declare Sub RetrySubRequests
Declare Sub SetCalcTempType (CalcVar As String, NewType As String)
Declare Function SetStringPointers (V1 As String, V2 As String, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer
DECLARE SUB ShowBlock (BlockIn As String)
Declare SUB SplitLines (CompSub As SubType Pointer)
Declare Function TempRemove(Removed As String) As String
Declare Sub TidyProgram
Declare Sub TidySubroutine(CompSub As SubType Pointer)
Declare Function TranslateFile(InFile As String) As String
Declare FUNCTION TypeOfVar (VarName As String, CurrSub As SubType Pointer) As String
Declare FUNCTION TypeOfValue (ValueNameIn As String, CurrentSub As SubType Pointer, SingCharString As Integer = 0) As String
Declare Sub UpdateOutgoingCalls (CompSub As SubType Pointer)
Declare Sub UpgradeCalcVar (VarName As String, VarType As String)
Declare Sub ValueChanged(VarName As String, VarValue As String)
DECLARE FUNCTION VarAddress (ArrayNameIn As String, CurrSub As SubType Pointer) As VariableType Pointer
Declare Sub WriteAssembly
Declare Sub WriteCompilationReport
DECLARE SUB WriteErrorLog

'Subs in assembly.bi
Declare Sub AsmOptimiser (CompSub As SubType Pointer)
DECLARE FUNCTION AsmTidy (DataSource As String) As String
DECLARE SUB AssembleProgram
Declare Sub BuildAsmSymbolTable
Declare FUNCTION IsASM (DataSource As String, ParamCount As Integer = -1) As Integer
Declare Function IsASMConst (DataSource As String) As Integer

'Subs in variables.bi
Declare Sub AddVar(VarNameIn As String, VarTypeIn As String, VarSizeIn As Integer, VarSubIn As SubType Pointer, VarPointerIn As String, OriginIn As String, FixedLocation As Integer = -1, ExplicitDeclaration As Integer = 0)
DECLARE SUB AllocateRAM
Declare Function CalcAliasLoc(LocationIn As String) As Integer
Declare Function GetWholeSFR(BitName As String) As String
Declare Function HasSFR(SFRName As String) As Integer
Declare Function HasSFRBit(BitName As String) As Integer
Declare Sub MakeSFR (UserVar As String, SFRAddress As Integer)

'Subs in preprocessor.bi
Declare Function CheckSysVarDef(ConditionIn As String) As String
DECLARE SUB PrepareBuiltIn ()
DECLARE SUB PreProcessor ()
DECLARE SUB RemIfDefs ()
DECLARE SUB ReplaceConstants ()
Declare Function ReplaceConstantsLine (DataSourceIn As String, IncludeStartup As Integer) As String
DECLARE SUB RunScripts ()

'Subs in utils.bi
Declare Function AddFullPath(CurrPath As String, FullPathIn As String = "") As String
DECLARE SUB Calculate (SUM As String)
DECLARE FUNCTION CountOccur (Source As String, Search As String, SearchWhole As Integer = 0) As Integer
Declare Function DelType (InString As String) As String
DECLARE FUNCTION GetByte (DataSource As String, BS As Integer) As String
Declare Function GetString(StringName As String, UsedInProgram As Integer = -1) As String
Declare Sub GetTokens(InData As String, OutArray() As String, ByRef OutSize As Integer, DivChar As String = "", IncludeDividers As Integer = 0)
Declare Function GetTypeLetter(InType As String) As String
Declare Function GetTypeSize(InType As String) As Integer
DECLARE FUNCTION IsCalc (Temp As String) As Integer
Declare FUNCTION IsCalcDivider (Temp As String) As Integer
DECLARE FUNCTION IsConst (Temp As String) As Integer
DECLARE FUNCTION IsDivider (Temp As String) As Integer
Declare Function IsIntType(InType As String) As Integer
DECLARE FUNCTION IsLet(Temp As String) As Integer
Declare Function IsValidValue(InValue As LongInt, TypeIn As String) As Integer
DECLARE FUNCTION MakeDec (DataSource As String) As LongInt
DECLARE SUB Replace (DataVar As String, Find As String, Rep As String)
Declare SUB ReplaceAll (DataVar As String, Find As String, Rep As String)
Declare Function ReplaceToolVariables(InData As String, FNExtension As String = "", FileNameIn As String = "", Tool As ExternalTool Pointer = 0) As String
DECLARE SUB SCICONV (STemp As String)
Declare Function ShortFileName(InName As String) As String
DECLARE FUNCTION ShortName (NameIn As String) As String
Declare Function LinkedListInsert OverLoad (Location As LinkedListElement Pointer, NewLine As String) As LinkedListElement Pointer
Declare Function LinkedListInsert OverLoad (Location As LinkedListElement Pointer, NewData As Any Pointer) As LinkedListElement Pointer
Declare Function LinkedListInsertList (Location As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
Declare Function LinkedListAppend (ListIn As LinkedListElement Pointer, NewList As LinkedListElement Pointer, NewListEndIn As LinkedListElement Pointer = 0) As LinkedListElement Pointer
Declare Function LinkedListDelete (Location As LinkedListElement Pointer) As LinkedListElement Pointer
Declare Function LinkedListDeleteList (StartLoc As LinkedListElement Pointer, EndLoc As LinkedListElement Pointer) As LinkedListElement Pointer
Declare Sub LinkedListPrint(StartNode As LinkedListElement Pointer)
Declare Function LinkedListSize(StartNode As LinkedListElement Pointer) As Integer
Declare Function SubSigMatch (SubSigIn As String, CallSigIn As String) As Integer
DECLARE FUNCTION WholeINSTR (DataIn As String, FindIn As String, SearchAgain As Integer = -1) As Integer
Declare Function WholeInstrLoc(DataSource As String, FindTemp As String) As Integer
DECLARE SUB WholeReplace (DataVar As String, Find As String, Rep As String)

'Initialise
'Misc Vars
DIM SHARED As Integer APC, ICC, DFC, FVLC, FRLC, FALC, SBC, SLC, IFC, WSC, FLC, DLC, SSC, SASC, POC
DIM SHARED As Integer COC, BVC, PCC, CVCC, TCVC, CAAC, ISRC, IISRC, RPLC, ILC, SCT
DIM SHARED As Integer CSC, CV, COSC, MemSize, FreeRAM, FoundCount, PotFound, IntLevel
DIM SHARED As Integer ChipRam, ConfWords, DataPass, ChipFamily, PSP, ChipProg
Dim Shared As Integer ChipPins
Dim Shared As Integer MainProgramSize, StatsUsedRam, StatsUsedProgram
DIM SHARED As Integer VBS, SVC, SVBC, MSGC, PreserveMode, ConstReplaced, SubCalls
DIM SHARED As Integer UserInt, PauseOnErr, USDC, MRC, GCGB, ALC, DCOC
DIM SHARED As Integer SubSizeCount, PCUpper, Bootloader, HighFSR, NoBankLocs
DIM SHARED As Integer RegCount, IntCount, AllowOverflow, SysInt, HMult, AllowInterrupt
Dim Shared As Integer ToolCount, ChipEEPROM, DataTables, ProgMemPages, PauseAfterCompile
Dim Shared As Integer USDelaysInaccurate, IntOscSpeeds, PinDirShadows
Dim Shared As Single ChipMhz, ChipMaxSpeed, StartTime, FileConverters

'Assembler vars
DIM SHARED As Integer ASPC, ASMCC, ASMSym, ToAsmSymbols

'Code Array
Dim Shared CompilerOutput As CodeSection Pointer
DIM SHARED ASMPROG(40000) As String: ASPC = 0

'Sub arrays
Dim Shared Subroutine(10000) As SubType Pointer: SBC = 0

'Processing Arrays
DIM SHARED gcDEF(1000, 1 TO 3) As String: DFC = 0
Dim SHARED gcINC(100, 1 TO 2) As String: ICC = 0
DIM SHARED TempData(300) As String
DIM SHARED CheckTemp(300) As String
Dim SHARED SysVars(1000) As SysVarType
DIM SHARED SysVarBits(8000) As SysVarType '1 - name, 2 - location, 3 - var
DIM SHARED FILE(300) As String
Redim SHARED FreeMem(1) As Integer
Redim SHARED VarLoc(1) As Integer
DIM SHARED MemRanges(20) As String: MRC = 0
Dim Shared NoBankLoc(10) As DataMemBankType: NoBankLocs = 0
Dim SHARED StringStore(0 To 1000) As ProgString: SSC = 0: SASC = 0
Dim Shared ConfigOps(200) As ConfigOp: COC = 0
Dim Shared ConfigSettings As LinkedListElement Pointer
Dim Shared EqConfigSettings As LinkedListElement Pointer
Dim Shared ChipConfigCode As CodeSection Pointer
DIM SHARED DefCONFIG(200) As String: DCOC = 0
DIM SHARED ConfigMask(20) As Integer
DIM SHARED DataTable(50) As DataTableType: DataTables = 0
DIM SHARED Messages(1 TO 2, 200) As String: MSGC = 0
DIM SHARED ASMCommands (200) As ASMCommand: ASMCC = 0
DIM SHARED ASMSymbols(10000, 1 TO 2) As String: ASMSym = 0
Dim Shared AsmSymbolList As LinkedListElement Pointer
Dim Shared ToAsmSymbol(500, 1 To 2) As String: ToAsmSymbols = 0
DIM SHARED FinalVarList(8000) As VariableListElement: FVLC = 0
DIM Shared FinalRegList(100) As VariableListElement: FRLC = 0
DIM Shared FinalAliasList(200) As VariableListElement: FALC = 0
DIM SHARED PreserveCode(20000) As String: PCC = 0
Dim Shared CalcVars(100) As CalcVar: TCVC = 0
Dim Shared As String AllSysSubs, UsedSysSubs
Dim Shared RegList(64, 2) As String: RegCount = 0
Dim Shared CalcAtAsm(2000) As String: CAAC = 0
Dim Shared CalcTempType(3) As String
Dim Shared Interrupts(200) As IntData: IntCount = 0
Dim Shared IntInitCode As CodeSection Pointer
Dim Shared IntHandlerCode As CodeSection Pointer
Dim Shared Tool(20) As ExternalTool: ToolCount = 0
Dim Shared ProgMemPage(MAX_PROG_PAGES) As ProgMemPageType: ProgMemPages = 0
Dim Shared IntOscSpeed(20) As Double: IntOscSpeeds = 0
Dim Shared FileConverter(50) As FileConverterType: FileConverters = 0
Dim Shared AttemptedCallList As LinkedListElement Pointer
Dim Shared OutConfig(4) As String
Dim Shared PinDirections As LinkedListElement Pointer
Dim Shared PinDirShadow(20) As String

Dim Shared As String Star80

Dim Shared As String ChipName, OSCType, CONFIG, Intrpt, gcOPTION
Dim Shared As String FI, OFI, HFI, ID, Version, ProgDir, CLD, LabelEnd
Dim Shared As String PrgExe, PrgParams, PrgDir, AsmExe, AsmParams
Dim Shared As ExternalTool Pointer AsmTool, PrgTool
Dim Shared As String CompReportFormat

#Define MAX_OUTPUT_MESSAGES 200
Dim Shared As String OutMessage(MAX_OUTPUT_MESSAGES)
Dim Shared As Integer OutMessages, ErrorsFound

Dim As Integer CD, T, PD

'Other GCBASIC source files
#include "utils.bi"
#include "assembly.bi"
#include "preprocessor.bi"
#include "variables.bi"

'FreeBASIC libraries
#include "file.bi"
#Include "string.bi"

'Close any open files, delete old error log
CLOSE
IF Dir("ERRORS.TXT") <> "" THEN KILL "ERRORS.TXT"

'Get random number seed
Randomize Timer

'Set version
Version = "0.9 22/9/2013"

'Initialise assorted variables
Star80 = ";********************************************************************************"
SysInt = 0
IntLevel = 0
AllowInterrupt = 0
ErrorsFound = 0
PinDirShadows = 0
MainProgramSize = 0
StatsUsedRam = 0
StatsUsedProgram = 0

'Various size counters
USDC = 0 'US delay loops
RPLC = 0 'Repeat loops
WSC = 0 'Wait Until/While loops
DLC = 0 'Do loops
SCT = 0 'Select Case
USDelaysInaccurate = 0 'Set if variable len US delays will be wrong

'Show startup messages, and read COMMAND
StartTime = Timer
InitCompiler

'Initialise code sections and lists
CompilerOutput = NewCodeSection
IntInitCode = NewCodeSection
IntHandlerCode = NewCodeSection
ChipConfigCode = NewCodeSection

AttemptedCallList = LinkedListCreate
ConfigSettings = LinkedListCreate
PinDirections = LinkedListCreate

'Load files and tidy them up
PreProcessor

'Compile
CompileProgram

'Allocate RAM
IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("AllocateRAM")
AllocateRAM

IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("TidyCode")
TidyProgram

'Combine subs
IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("LocatingSubs")
MergeSubroutines

'Final optimisation
FinalOptimise

'Determine the correct setting for the CONFIG directive
If ConfWords > 0 Then
	IF VBS = 1 THEN PRINT SPC(5); Message("CalcConfig")
	CalcConfig
End If

If VBS = 1 THEN PRINT : PRINT SPC(5); Message("WritingASM")
WriteAssembly

'If no errors, show success message and assemble
IF Not ErrorsFound THEN
	'Success message
	PRINT Message("Success");
	IF VBS = 0 THEN
		PRINT
	Else
		Dim Temp As String
		Temp = Trim(Str(TIMER - StartTime))
		IF LEN(Temp) > 4 Then Temp = Left(Temp, 5)
		PRINT Message("CompTime") + Temp + Message("CompSecs")
		PRINT
		PRINT Message("Summary")
		PRINT SPC(5); Message("DataRead")
		PRINT SPC(10); Message("InLines") + Str(MainProgramSize)
		PRINT SPC(10); Message("Vars") + Str(FVLC)
		PRINT SPC(10); Message("Consts") + Str(DFC)
		PRINT SPC(10); Message("Subs") + Str(SBC)
		PRINT SPC(5); Message("ChipUsage")
		Temp = Message("UsedProgram")
		Replace Temp, "%used%", Str(StatsUsedProgram)
		Replace Temp, "%total%", Str(ChipProg)
		If ChipProg <> 0 Then Temp += Format(StatsUsedProgram / ChipProg, " (###.##%)")
		PRINT SPC(10); Temp
		Temp = Message("UsedRAM")
		Replace Temp, "%used%", Str(StatsUsedRam)
		Replace Temp, "%total%", Str(ChipRAM)
		If ChipRAM <> 0 Then Temp += Format(StatsUsedRAM / ChipRAM, " (###.##%)")
		PRINT SPC(10); Temp
	END IF
	
	'Assemble program
	If AsmExe <> "" Then
		PRINT
		PRINT Message("MakeASM")
		
		'Internal assembler
		IF UCase(AsmExe) = "GCASM" THEN
			AssembleProgram
			IF Not ErrorsFound THEN PRINT Message("ASMSuccess")
		
		'Assemble program with external assembler
		Else
			ExtAssembler
			
		END IF
	End If
End If

'Write errors to file
WriteErrorLog

'Write compilation report
WriteCompilationReport

'Download program
IF PrgExe <> "" AND AsmExe <> "" AND Not ErrorsFound THEN
	PRINT
	PRINT Message("SendToPIC")
	PrgExe = ReplaceToolVariables(PrgExe, "hex",, PrgTool)
	PrgParams = ReplaceToolVariables(PrgParams, "hex",, PrgTool)
	IF VBS = 1 THEN PRINT SPC(5); Message("Calling") + PrgExe
	
	Dim As String SaveCurrDir
	SaveCurrDir = CurDir
	If PrgDir <> "" Then ChDir ReplaceToolVariables(PrgDir, "hex")
	
	PD = Exec(PrgExe, PrgParams)
	'SHELL Chr(34) + SendToPIC + Chr(34)
	
	ChDir SaveCurrDir
END IF

'End of program
'Pause and wait for key at end of compilation?
If PauseAfterCompile Then
	Print
	Print Message("AnyKey")
	GetKey
End If
End

SUB Add18FBanks(CompSub As SubType Pointer)
	Dim As String TempData, VarName
	Dim As Integer PD, ConstFound, FC
	
	Dim As LinkedListElement Pointer CurrLine
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		TempData = LCase(CurrLine->Value)
		
		IF INSTR(TempData, "movff") = 0 AND INSTR(TempData, "lfsr") = 0 AND INSTR(TempData, "retfie") = 0 AND INSTR(TempData, ",access") = 0 AND INSTR(TempData, ", access") = 0 AND TempData <> "" AND Left(TempData, 1) <> ";" THEN
			TempData = Trim(UCase(CurrLine->Value))
			VarName = Mid(TempData, INSTR(TempData, " ") + 1)
			IF INSTR(VarName, ",") <> 0 THEN VarName = Left(VarName, INSTR(VarName, ",") - 1)
			VarName = UCase(Trim(VarName))
			TempData = Left(TempData, INSTR(TempData, " ") - 1)
			IF INSTR(TempData, "F") <> 0 THEN
				'Check if the variable being accessed is a SFR, and add banking mode
				If IsInAccessBank(VarName) Then
					CurrLine->Value = CurrLine->Value + ",ACCESS"
				Else
					CurrLine->Value = CurrLine->Value + ",BANKED"
				End If
			END IF
		END If
		CurrLine = CurrLine->Next
	Loop
	
END SUB

SUB AddBankCommands(CompSub As SubType Pointer)
	'Scans through program, adds banksel instructions where needed
	
	'Not used for AVR
	If ModeAVR Then Exit Sub
	
	Dim As Integer BankMask, Bank, FindVar, BankselNeeded, ReturnAdded
	Dim As String TempData, OtherData, VarInBank, VarName, DestLabel
	Dim As LinkedListElement Pointer CurrLine, LabelList, LabelListPos, LabelLoc, BankList, BankListLoc, PrintLoc
	Dim As ProgLineMeta Pointer CurrMeta, DestMeta
	
	'Get bank size
	Select Case ChipFamily
		Case 12: BankMask = 32
		Case 14: BankMask = 384
		Case 15: BankMask = 3968
		Case 16: BankMask = 65280
	End Select
	
	'Add return to end of sub
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		If CurrLine->Next = 0 Then
			LinkedListInsert(CurrLine, " return")
			ReturnAdded = -1
			CurrLine = 0
		Else
			CurrLine = CurrLine->Next
		End If 
	Loop
	
	'Get list of labels and locations in program
	LabelList = GetLabelList(CompSub)
	
	'Check required bank for each line
	'Also, ensure metadata attached
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		'Get metadata for line
		CurrMeta = GetMetaData(CurrLine)
		
		'Check to see what bank line requires
		TempData = CurrLine->Value
		
		Bank = -1
		VarInBank = ""
		If Left(Trim(TempData), 6) = "return" Then
			Bank = 0
		ElseIf Left(Trim(TempData), 5) = "call " Then
			Bank = 0
			
		Else
			'If line operates on file registers, check var usage
			OtherData = UCase(Trim(TempData))
			IF INSTR(OtherData, " ") <> 0 THEN OtherData = Left(OtherData, INSTR(OtherData, " ") - 1)
			IF INSTR(OtherData, "F") <> 0 And WholeINSTR(TempData, "lfsr") <> 2 And WholeINSTR(TempData, "movff") <> 2 THEN
				
				'Check for SFRs outside of Bank 0
				For FindVar = 1 TO SVC
					If WholeINSTR(TempData, SysVars(FindVar).Name) = 2 Then
						'Found an SFR
						
						'18F code: check for SFRs outside of access bank
						If ChipFamily = 16 Then
							If Not IsNonBanked(SysVars(FindVar).Location) Then
								Bank = SysVars(FindVar).Location And BankMask
								VarInBank = SysVars(FindVar).Name
							End If
						
						'16F1 code: Don't need bank selection for 12 core registers on 16F1xxx
						ElseIf ChipFamily = 15 Then
							If (SysVars(FindVar).Location And Not BankMask) >= 12 Then
								Bank = SysVars(FindVar).Location And BankMask
								VarInBank = SysVars(FindVar).Name
							End If
							
						'Other PIC code: assume always need bank selection
						Else
							Bank = SysVars(FindVar).Location And BankMask
							VarInBank = SysVars(FindVar).Name
						
						End If
						Exit For
					End If
				Next
				
				'Check for user vars outside of bank 0
				TempData = Trim(UCase(TempData))
				FOR FindVar = 1 TO FVLC
					With FinalVarList(FindVar)
						VarName = UCase(Trim(.Name))
						IF WholeINSTR(TempData, VarName) = 2 Then
							'Have found a variable, is banking needed?
							IF Not .IsArray AND Not IsRegister(VarName) Then
								Bank = VAL(.Value) AND BankMask
								VarInBank = VarName
							End If
							Exit For
						End If
					End With
				Next
				
			End If
		End If
		
		With *CurrMeta
			.RequiredBank = Bank
			.VarInBank = VarInBank
		End With
		
		CurrLine = CurrLine->Next
	Loop
	
	'Record jump to first line
	'Use null pointer to indicate it
	CurrLine = CompSub->CodeStart->Next
	If CurrLine <> 0 Then
		DestMeta = CurrLine->MetaData
		LinkedListInsert(DestMeta->PrevCommands, 0)
	End If
	
	'Generate graph of connections between lines
	CurrLine = CompSub->CodeStart->Next
	Dim As String CurrLine4, CurrLine5, CurrLine6, CurrLine8
	Do While CurrLine <> 0
		
		CurrLine4 = Left(CurrLine->Value, 4)
		CurrLine5 = Left(CurrLine->Value, 5)
		CurrLine6 = Left(CurrLine->Value, 6)
		CurrLine8 = Left(CurrLine->Value, 8)
		CurrMeta = CurrLine->MetaData
		
		'goto/bra
		If CurrLine6 = " goto " Or CurrLine5 = " bra " Then
			'Print CurrLine->Value; " to label"
			
			'Get label jumped to
			'Get label name
			If CurrLine6 = " goto " Then
				DestLabel = LCase(Trim(Mid(CurrLine->Value, 7)))
			Else
				DestLabel = LCase(Trim(Mid(CurrLine->Value, 6)))
			End If
			
			'Is this a delay jump?
			TempData = DestLabel
			Do While InStr(TempData, " ") <> 0
				Replace TempData, " ", ""
			Loop
			If TempData = "$+1" Or TempData = "$+2" Then
				'Record jump to next line
				LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next)
				If CurrLine->Next <> 0 Then
					DestMeta = CurrLine->Next->MetaData
					LinkedListInsert(DestMeta->PrevCommands, CurrLine)
				End If
				
			'Not a delay jump, find destination
			Else
				'Find label name in list
				LabelLoc = 0
				LabelListPos = LabelList->Next
				Do While LabelListPos <> 0
					If LCase(LabelListPos->Value) = DestLabel Then
						LabelLoc = LabelListPos->MetaData
						Exit Do
					End If
					LabelListPos = LabelListPos->Next
				Loop
				'Record jump
				If LabelLoc <> 0 Then
					'Record in list of next locations for current line
					LinkedListInsert(CurrMeta->NextCommands, LabelLoc)
					'Record in list of previous locations for destination line
					DestMeta = LabelLoc->MetaData
					LinkedListInsert(DestMeta->PrevCommands, CurrLine)
				End If
				
			End If
						
		'cpfseq/cpfslt/cpfsgt
		ElseIf CurrLine5 = " cpfs" Then
			'Print CurrLine->Value; " to next or label"
			'Show jump to next
			LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next)
			If CurrLine->Next <> 0 Then
				DestMeta = CurrLine->Next->MetaData
				LinkedListInsert(DestMeta->PrevCommands, CurrLine)
			End If
			
			'Get label jumped to
			'Get label name
			DestLabel = LCase(Trim(Mid(CurrLine->Value, 9)))
			'Find label name in list
			LabelLoc = 0
			LabelListPos = LabelList->Next
			Do While LabelListPos <> 0
				If LCase(LabelListPos->Value) = DestLabel Then
					LabelLoc = LabelListPos->MetaData
					Exit Do
				End If
				LabelListPos = LabelListPos->Next
			Loop
			'Record jump
			If LabelLoc <> 0 Then
				LinkedListInsert(CurrMeta->NextCommands, LabelLoc)
				DestMeta = LabelLoc->MetaData
				LinkedListInsert(DestMeta->PrevCommands, CurrLine)
			End If
			
		'btfsc/btfss
		ElseIf CurrLine5 = " btfs" Or CurrLine8 = " decfsz " Or _
			    CurrLine8 = " incfsz " Or CurrLine8 = " dcfsnz " Or _
			    CurrLine8 = " infsnz " Or CurrLine8 = " tstfsz" Or _
			    CurrLine4 = " bc " Or CurrLine4 = " bn " Or _
			    CurrLine4 = " bz " Or CurrLine5 = " bnc " Or _
			    CurrLine5 = " bnn " Or CurrLine5 = " bnz " Or _
			    CurrLine5 = " bov " Or CurrLine6 = " bnov " Then
			'Print CurrLine->Value; " to next or after next"
			'Show jump to next
			LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next)
			If CurrLine->Next <> 0 Then
				'Show jump from this
				DestMeta = CurrLine->Next->MetaData
				LinkedListInsert(DestMeta->PrevCommands, CurrLine)
				
				'Show jump after next
				LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next->Next)
				If CurrLine->Next->Next <> 0 Then
					DestMeta = CurrLine->Next->Next->MetaData
					LinkedListInsert(DestMeta->PrevCommands, CurrLine)
				End If
			End If
			
		'Return
		ElseIf Left(CurrLine->Value, 7) = " return" Then
			'Don't add any jump
		
		'others
		Else
			'Print CurrLine->Value; " to next"
			'Show jump to next
			LinkedListInsert(CurrMeta->NextCommands, CurrLine->Next)
			If CurrLine->Next <> 0 Then
				DestMeta = CurrLine->Next->MetaData
				LinkedListInsert(DestMeta->PrevCommands, CurrLine)
			End If
		End If
		
		CurrLine = CurrLine->Next
	Loop
	
	'Print prev/next
	/' If CompSub->Name = "Interrupt" Then
		CurrLine = CompSub->CodeStart->Next
		Color 15
		Do While CurrLine <> 0
			Print "Line:"; CurrLine->Value
			CurrMeta = CurrLine->MetaData
			
			PrintLoc = CurrMeta->PrevCommands->Next
			Do While PrintLoc <> 0
				BankListLoc = PrintLoc->MetaData
				Print "    After:"; BankListLoc->Value
				PrintLoc = PrintLoc->Next
			Loop
			PrintLoc = CurrMeta->NextCommands->Next
			Do While PrintLoc <> 0
				BankListLoc = PrintLoc->MetaData
				Print "    Before:"; BankListLoc->Value 
				PrintLoc = PrintLoc->Next
			Loop
			CurrLine = CurrLine->Next
		Loop
		Color 7
	End If
	'/
	
	'Find locations where current bank may not match required bank
	CurrLine = CompSub->CodeStart->Next
	'FinalBankselNeeded = 0
	Do While CurrLine <> 0
		
		'Decide if banksel needed
		BankselNeeded = 0
		VarInBank = ""
		
		CurrMeta = CurrLine->MetaData
		'Need to make sure metadata exists for line, banksels won't have it
		If CurrMeta <> 0 Then
			'Does line require a particular bank?
			If CurrMeta->RequiredBank <> -1 Then
				
				'Need to check what bank/s chip will be in
				BankList = FindPotentialBanks(CurrLine)
				
				'If it can only be in correct bank before line, no need for banksel
				If LinkedListSize(BankList) <> 1 Then
					If LinkedListSize(BankList) <> 0 Or CurrMeta->RequiredBank > 0 Then
						BankselNeeded = -1
					End If
					
				ElseIf BankList->Next->NumVal <> CurrMeta->RequiredBank Then
					BankselNeeded = -1
				End If
				
				If BankselNeeded Then
					VarInBank = CurrMeta->VarInBank
					If VarInBank = "" Then
						If CurrMeta->RequiredBank = 0 Then
							If ChipFamily = 16 Then
								VarInBank = "0"
							Else
								VarInBank = "STATUS"
							End If
						Else
							Print "Internal error in AddBankCommands"
						End If
					End If
				End If
			End If
		End If
		
		'Add banksel
		If BankselNeeded Then
			LinkedListInsert(CurrLine->Prev, " banksel " + VarInBank)
		End If
		''Add final banksel, need to return in bank 0
		'If CurrLine->Next = 0 And FinalBankselNeeded Then
		'	LinkedListInsert(CurrLine, " banksel STATUS")
		'	CurrLine = 0
		'End If
		
		If CurrLine->Next = 0 Then
			'Remove added return
			If ReturnAdded Then
				If CurrLine->Value = " return" Then
					LinkedListDelete(CurrLine)
					CurrLine = 0
				End If
			End If
		End If
		
		If CurrLine <> 0 Then
			CurrLine = CurrLine->Next
		End If
	Loop
	
End Sub

Sub AddPageCommands(CompSub As SubType Pointer)
	
	Dim As LinkedListElement Pointer CurrLine, NextLine
	Dim As Integer CalledSub, CalledSubPage, ThisSubPage, TempDataCount
	Dim As Integer RestorePage, ForceRestorePage, NextCalledSub, NextCalledSubPage
	Dim As String TempData(100), CallTarget, ThisSubName, NextCallTarget
	Dim As SubType Pointer GotoSub
	
	If CompSub = 0 Then Exit Sub
	
	ThisSubPage = CompSub->DestPage
	
	'This is not needed on AVR or PIC18
	If ModeAVR Then Exit Sub
	If ModePIC And ChipFamily = 16 Then Exit Sub
	
	'Remove any existing page selection commands
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		'Get metadata for line
		If CurrLine->MetaData <> 0 Then
			'Line is automatic page sel, remove it
			If GetMetaData(CurrLine)->IsAutoPageSel Then
				CurrLine = LinkedListDelete(CurrLine)
			End If
		End If
				
		CurrLine = CurrLine->Next
	Loop
	
	'Search subroutine for calls
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		'Found a call, get destination and destination page
		If Left(CurrLine->Value, 6) = " call " Then
			GetTokens(CurrLine->Value, TempData(), TempDataCount)
			CallTarget = TempData(2)
			ThisSubName = CompSub->Name
			CalledSub = LocationOfSub(CallTarget, "")
			CalledSubPage = Subroutine(CalledSub)->DestPage
			
			'Check next line for a call, get dest and dest page
			'(Decide if page needs to be restored after this call) 
			RestorePage = -1
			'Find next actual code line, ignore comments
			NextLine = CurrLine->Next
			Do While (Left(NextLine->Value, 1) = ";" Or Left(NextLine->Value, 8) = "PRESERVE") AND NextLine->Next <> 0
				NextLine = NextLine->Next
			Loop
			If NextLine <> 0 Then
				If Left(NextLine->Value, 6) = " call " Then
					'Next line is a call, find the sub it is calling and what page it is on
					GetTokens(NextLine->Value, TempData(), TempDataCount)
					NextCallTarget = TempData(2)
					NextCalledSub = LocationOfSub(NextCallTarget, "")
					NextCalledSubPage = Subroutine(NextCalledSub)->DestPage
					If NextCalledSubPage <> ThisSubPage Then
						RestorePage = 0
					End If
				End If
			End If
			
			'Is call in different page?
			If CalledSubPage = ThisSubPage Then
				RestorePage = 0
			End If
			
			'If called sub ends in goto, restore
			'May have linked list: called sub > goto sub > another goto sub
			'Need to check if called sub's goto, or any other goto will change page
			'GotoSub = Subroutine(NextCalledSub)
			GotoSub = Subroutine(CalledSub)
			Do While GotoSub -> HasFinalGoto
				GotoSub = Subroutine(LocationOfSub(GotoSub->FinalGotoDest, ""))
				If CompSub->DestPage <> GotoSub->DestPage Then
					RestorePage = -1
					Exit Do
				End If
			Loop
			
			'Alter call to correct instruction/s
			If ModePIC Then
				If ChipFamily = 12 Or ChipFamily = 14 Or ChipFamily = 15 Then
					'Remove call temporarily
					CurrLine = LinkedListDelete(CurrLine)
					'Set page if needed
					If CalledSubPage <> ThisSubPage Then
						CurrLine = LinkedListInsert(CurrLine, " pagesel " + CallTarget)
						GetMetaData(CurrLine)->IsAutoPageSel = -1
					End If
					'Add call back
					CurrLine = LinkedListInsert(CurrLine, " call " + CallTarget)
					'Restore PCLATH if needed
					If RestorePage Then
						CurrLine = LinkedListInsert(CurrLine, " pagesel $")
						GetMetaData(CurrLine)->IsAutoPageSel = -1
					End If
				End If
			ElseIf ModeZ8 Then
				CurrLine = LinkedListDelete(CurrLine)
				CurrLine = LinkedListInsert(CurrLine, " call " + CallTarget)
			End If
			
		'Also deal with gotos that jump to other subs
		ElseIf Left(CurrLine->Value, 6) = " goto " And CompSub->HasFinalGoto Then
			GetTokens(CurrLine->Value, TempData(), TempDataCount)
			
			'Have found goto, make sure it is a jump to another sub
			CallTarget = TempData(2)
			If LCase(CallTarget) = LCase(CompSub->FinalGotoDest) Then
				ThisSubName = CompSub->Name
				CalledSub = LocationOfSub(CallTarget, "")
				If CalledSub <> 0 Then
					'Have found a jump to a sub
					CalledSubPage = Subroutine(CalledSub)->DestPage
					
					'Alter call to correct instruction/s
					If ModePIC Then
						If ChipFamily = 12 Or ChipFamily = 14 Or ChipFamily = 15 Then
							CurrLine = LinkedListDelete(CurrLine)
							If CalledSubPage = ThisSubPage Then
								CurrLine = LinkedListInsert(CurrLine, " goto " + CallTarget)
							Else
								CurrLine = LinkedListInsert(CurrLine, " pagesel " + CallTarget)
								GetMetaData(CurrLine)->IsAutoPageSel = -1
								CurrLine = LinkedListInsert(CurrLine, " goto " + CallTarget)
							End If
						End If
					End If
				End If
			End If
		End If
		
		
		CurrLine = CurrLine->Next
	Loop
	
End Sub

Sub AddMainEndCode
	Dim As LinkedListElement Pointer CurrPos
	
	'Prevent return being placed at end
	Subroutine(0)->NoReturn = -1
	
	'Get last location in main sub
	CurrPos = Subroutine(0)->CodeStart
	Do While CurrPos->Next <> 0
		CurrPos = CurrPos->Next
	Loop
	
	If ModePIC Then
		CurrPos = LinkedListInsert(CurrPos, "BASPROGRAMEND")
		CurrPos = LinkedListInsert(CurrPos, " sleep")
		'CurrPos = LinkedListInsert(CurrPos, " goto $")
		CurrPos = LinkedListInsert(CurrPos, " goto BASPROGRAMEND")
	ElseIf ModeAVR Then
		CurrPos = LinkedListInsert(CurrPos, "BASPROGRAMEND:")
		CurrPos = LinkedListInsert(CurrPos, " sleep")
		'CurrPos = LinkedListInsert(CurrPos, "ProgEndLoop:")
		'CurrPos = LinkedListInsert(CurrPos, " rjmp ProgEndLoop")
		CurrPos = LinkedListInsert(CurrPos, " rjmp BASPROGRAMEND")
	ElseIf ModeZ8 Then
		
	Else
		Print "Unsupported family, Main"
		
	End If
End Sub

Sub AddMainInitCode
	'Add initialisation code to start of Main routine
	Dim As LinkedListElement Pointer CurrLine
	Dim As Integer CurrInc, SubLoc, OldVBS
	Dim As String Temp
	
	Dim As SubType Pointer AutoDirSub
	
	CurrLine = Subroutine(0)->CodeStart
	
	'Mark Main sub as destined for first page
	Subroutine(0)->FirstPage = -1
	
	'Set up stack (AVR and Z8)
	If ModeAVR Then
		CurrLine = LinkedListInsert(CurrLine, ";Initialise stack")
		If HasSFR("SPH") Then
			CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,high(RAMEND)")
			CurrLine = LinkedListInsert(CurrLine, " out SPH, SysValueCopy")
		End If
		CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,low(RAMEND)")
		CurrLine = LinkedListInsert(CurrLine, " out SPL, SysValueCopy")
	ElseIf ModeZ8 Then
		CurrLine = LinkedListInsert(CurrLine, ";Initialise stack")
		CurrLine = LinkedListInsert(CurrLine, " ldx SPH, #HIGH(RAMEND)")
		CurrLine = LinkedListInsert(CurrLine, " ldx SPL, #LOW(RAMEND)")
	End If
	
	'Call init routines
	CurrLine = LinkedListInsert(CurrLine, ";Call initialisation routines")
	SubLoc = LocationOfSub("InitSys", "")
	'Change 7/10/2012: Init routines do not need to be in page 0
	'Subroutine(SubLoc)->FirstPage = -1
	If ModeAVR Then
		CurrLine = LinkedListInsert(CurrLine, " rcall INITSYS")
	Else
		CurrLine = LinkedListInsert(CurrLine, " call INITSYS")
	End If
	FOR CurrInc = 1 TO ICC
		Temp = Trim(gcINC(CurrInc, 2))
		If Temp <> "" And Left(Temp, 1) <> ";" Then
			SubLoc = LocationOfSub(Temp, "")
			'If SubLoc <> 0 Then Subroutine(SubLoc)->FirstPage = -1
			If ModeAVR Then
				CurrLine = LinkedListInsert(CurrLine, " rcall " + Temp)
			Else
				CurrLine = LinkedListInsert(CurrLine, " call " + Temp)
			End If
		End If
	Next
	
	'Enable interrupts
	If UserInt Or SysInt Then
		CurrLine = LinkedListInsert(CurrLine, ";Enable interrupts")
		If ModePIC Then
			CurrLine = LinkedListInsert(CurrLine, " bsf INTCON,GIE")
			If HasSFRBit("PEIE") Then
				CurrLine = LinkedListInsert(CurrLine, " bsf INTCON,PEIE")
			End If
			CurrLine = LinkedListInsert(CurrLine, " clrf SysIntOffCount")
			
		ElseIf ModeAVR Then
			CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
			CurrLine = LinkedListInsert(CurrLine, " sts SysIntOffCount,SysValueCopy")
			CurrLine = LinkedListInsert(CurrLine, " sei")
			
		ElseIf ModeZ8 Then
			CurrLine = LinkedListInsert(CurrLine, " ei")
			
		End If
	End If
	
	'Automatic pin direction setting
	'Create new subroutine to store dir commands
	'Add commands, compile sub, inline code, delete sub
	AutoDirSub = NewSubroutine("SysInitPinDirs")
	LinkedListInsertList(AutoDirSub->CodeStart, GenerateAutoPinDir)
	'If code generated, add it
	If AutoDirSub->CodeStart->Next <> 0 Then
		'Disable verbose mode to stop any output from CompileSubroutine
		OldVBS = VBS: VBS = 0
		'Compile auto pin dir sub
		CompileSubroutine(AutoDirSub)
		VBS = OldVBS
		'Add code from it to main
		CurrLine = LinkedListInsert(CurrLine, ";Automatic pin direction setting")
		CurrLine = LinkedListInsertList(CurrLine, AutoDirSub->CodeStart)
	End If
	
	'Comment start of main program
	CurrLine = LinkedListInsert(CurrLine, "")
	CurrLine = LinkedListInsert(CurrLine, ";Start of the main program")
	
End Sub

Sub AddInterruptCode
	Dim As Integer IntSubLoc, CurrVect
	Dim As LinkedListElement Pointer CurrLine, SubStart, SubEnd
	
	'If no interrupts are used, no need for any of this
	If Not (UserInt Or SysInt) Then Return
	
	'Get list of variables to save/restore
	'Need to check every handler to see what registers it uses
	Dim As LinkedListElement Pointer HandlerSubs, SaveVars, CurrCalled, SaveVarPos
	Dim As LinkedListElement Pointer SearchA, SearchB
	Dim As SubType Pointer HandlerSub, CalledSub
	Dim As String RegItem(100), TempData(20), Temp
	Dim As Integer HandlerID, RegItems, CurrReg, SV, CurrBit, DataCount, PCHUsed
	
	'Update list of sub calls
	RetrySubRequests
	
	'Make list of all variables that may need saving
	RegItems = 0
	RegItems += 1: RegItem(RegItems) = "DelayTemp"
	RegItems += 1: RegItem(RegItems) = "DelayTemp2"
	RegItems += 1: RegItem(RegItems) = "SysDivMultX"
	RegItems += 1: RegItem(RegItems) = "SysWaitTempMS"
	RegItems += 1: RegItem(RegItems) = "SysStringB"
	RegItems += 1: RegItem(RegItems) = "SysDivMultX_H"
	RegItems += 1: RegItem(RegItems) = "SysWaitTempMS_H"
	RegItems += 1: RegItem(RegItems) = "SysStringB_H"
	RegItems += 1: RegItem(RegItems) = "SysDivLoop"
	RegItems += 1: RegItem(RegItems) = "SysWaitTemp10MS"
	RegItems += 1: RegItem(RegItems) = "SysWaitTempS"
	RegItems += 1: RegItem(RegItems) = "SysWaitTempUS"
	RegItems += 1: RegItem(RegItems) = "SysWaitTemp10US"
	RegItems += 1: RegItem(RegItems) = "SysWaitTempM"
	RegItems += 1: RegItem(RegItems) = "SysStringLength"
	RegItems += 1: RegItem(RegItems) = "SysWaitTempUS_H"
	RegItems += 1: RegItem(RegItems) = "SysWaitTempH"
	RegItems += 1: RegItem(RegItems) = "SysDivMultA"
	RegItems += 1: RegItem(RegItems) = "SysStringA"
	RegItems += 1: RegItem(RegItems) = "SysDivMultA_H"
	RegItems += 1: RegItem(RegItems) = "SysStringA_H"
	RegItems += 1: RegItem(RegItems) = "SysDivMultB"
	RegItems += 1: RegItem(RegItems) = "SysReadA"
	RegItems += 1: RegItem(RegItems) = "SysDivMultB_H"
	RegItems += 1: RegItem(RegItems) = "SysReadA_H"
	RegItems += 1: RegItem(RegItems) = "SysSignByte"
	
	RegItems += 1: RegItem(RegItems) = "DataPointer"
	
	'Temporary calculation registers
	'Need to do A, B, X for SysCalcTemp, SysByteTemp, SysWordTemp, SysIntTemp, SysLongTemp
	Dim TempVarLetter(3) As String = {"A", "B", "X"}
	Dim TempVarType(5) As String = {"Calc", "Byte", "Word", "Integer", "Long"}
	Dim As Integer CurrLetter, CurrTypeNo, CurrByte
	Dim As String CurrType, SaveRegName
	For CurrLetter = 0 To 2
		For CurrTypeNo = 0 To 4
			CurrType = TempVarType(CurrTypeNo)
			If CurrType = "Calc" Then CurrType = "Long"
			For CurrByte = 1 To GetTypeSize(CurrType)
				RegItems += 1: RegItem(RegItems) = GetByte("Sys" + TempVarType(CurrTypeNo) + "Temp" + TempVarLetter(CurrLetter), CurrByte - 1)
			Next
		Next
	Next
	
	If ModePIC Then
		RegItems += 1: RegItem(RegItems) = "SysIFTemp"
		RegItems += 1: RegItem(RegItems) = "PCLATH"
		If ChipFamily = 12 Or ChipFamily = 14 Then
			RegItems += 1: RegItem(RegItems) = "FSR"
		Else
			RegItems += 1: RegItem(RegItems) = "FSR0"
			RegItems += 1: RegItem(RegItems) = "FSR0H"
			RegItems += 1: RegItem(RegItems) = "FSR1"
			RegItems += 1: RegItem(RegItems) = "FSR1H"
		End If
	End If
	
	'Add calc vars to list
	For SV = 1 to TCVC
		With CalcVars(SV)
			If .MaxType <> "" Then
				For CurrBit = 0 To GetTypeSize(.MaxType) - 1
					RegItems += 1: RegItem(RegItems) = GetByte("SysTemp" + Str(SV), CurrBit)
				Next
			End If
		End With
	Next
	
	'Get list for all vectors
	SaveVars = LinkedListCreate
	SaveVarPos = SaveVars
	
	'Search all handlers for registers
	'When CurrVect = 0, check for Interrupt sub
	For CurrVect = 0 To IntCount
		HandlerID = 0
		If CurrVect = 0 Then
			HandlerID = LocationOfSub("Interrupt", "")
		Else
			With Interrupts(CurrVect)
				If .Handler <> "" Then
					'Handler is defined for vector
					HandlerID = LocationOfSub(.Handler, "", "")
				End If
			End With
		End If
				
		If HandlerID > 0 Then
			'Get all subroutines called from handler
			HandlerSub = Subroutine(HandlerID)
			HandlerSubs = GetCalledSubs(HandlerSub)
			LinkedListInsert(HandlerSubs, HandlerSub)
			'Scan through all handler subs
			CurrCalled = HandlerSubs->Next
			Do While CurrCalled <> 0
				CalledSub = CurrCalled->MetaData
				'Search sub lines
				CurrLine = CalledSub->CodeStart->Next
				Do While CurrLine <> 0
					'Search line for variables to back up 
					CurrReg = 1
					Do While CurrReg <= RegItems
						If WholeINSTR(CurrLine->Value, RegItem(CurrReg)) = 2 Then
							SaveVarPos = LinkedListInsert(SaveVarPos, RegItem(CurrReg))
							'Remove register from list, no need to keep looking for it
							RegItem(CurrReg) = RegItem(RegItems)
							RegItems -= 1
							CurrReg -= 1
						End If
						CurrReg += 1
					Loop
					
					'Some PIC only values that will need saving
					If ModePIC Then
						If WholeINSTR(CurrLine->Value, "lfsr") <> 0 Then
							GetTokens(CurrLine->Value, TempData(), DataCount)
							If LCase(TempData(1)) = "lfsr" Then
								SaveVarPos = LinkedListInsert(SaveVarPos, "FSR" + TempData(2) + "L")
								SaveVarPos = LinkedListInsert(SaveVarPos, "FSR" + TempData(2) + "H")
							End If
						ElseIf WholeINSTR(CurrLine->Value, "AFSR") = 1 Then
							Temp = Mid(CurrLine->Value, InStr(UCase(CurrLine->Value), "AFSR"))
							If Mid(Temp, 7, 1) = "H" Then
								Temp = "FSR" + Mid(Temp, 5, 1) + Mid(Temp, 7, 1)
							Else
								Temp = "FSR" + Mid(Temp, 5, 1) + "L"
							End If
							SaveVarPos = LinkedListInsert(SaveVarPos, Temp)
						End If
					End If
					
					CurrLine = CurrLine->Next
				Loop
				CurrCalled = CurrCalled->Next
			Loop
			
		End If
	Next
	
	'Always save PCLATH
	PCHUsed = 0
	If ChipFamily = 14 Or ChipFamily = 15 Then
		If HasSFR("PCLATH") Then
			SaveVarPos = LinkedListInsert(SaveVarPos, "PCLATH")
			PCHUsed = -1
		End If
	End If
	
	'Delete variables that share location with another saved var
	SearchA = SaveVars->Next
	Do While SearchA <> 0
		SearchB = SearchA->Next
		Do While SearchB <> 0
			'A and B share location, remove B
			If GetRegisterLoc(SearchA->Value) = GetRegisterLoc(SearchB->Value) Then
				If (Left(SearchA->Value, 7) <> "SysTemp" And GetRegisterLoc(SearchA->Value) <> -1) Or _
					LCase(SearchA->Value) = LCase(SearchB->Value) Then
					SearchB = LinkedListDelete(SearchB)
				End If
			End If
			
			SearchB = SearchB->Next
		Loop
		SearchA = SearchA->Next
	Loop
	'Should now have a list of variables that need to be backed up before interrupt
	
	If ModePIC Then
		'On PIC, need to add context save and On Int to start of interrupt routine
		'and add context restore at the end
		
		'Get interrupt sub
		IntSubLoc = LocationOfSub("Interrupt", "")
		If IntSubLoc = 0 Then
			SBC += 1
			Subroutine(SBC) = NewSubroutine("Interrupt")
			IntSubLoc = SBC
			'Mark as required and compiled (then only insert asm, no BASIC allowed)
			With *Subroutine(SBC)
				'.Required = -1
				.Compiled = -1
			End With
		End If
		With *Subroutine(IntSubLoc)
			'Interrupt sub must go on first page
			.FirstPage = -1
			'Interrupt sub doesn't need return added
			.NoReturn = -1
			'Subroutine is required
			.Required = -1
			
			'Get code start
			SubStart = .CodeStart
		End With
		
		'Add context save code
		CurrLine = SubStart
		AddVar "SysW", "BYTE", 1, 0, "REAL", ""
		AddVar "SysSTATUS", "BYTE", 1, 0, "REAL", ""
		AddVar "SysIntOffCount", "BYTE", 1, 0, "REAL", ""
		'Variables to store registers
		SaveVarPos = SaveVars->Next
		Do While SaveVarPos <> 0
			AddVar("Save" + SaveVarPos->Value, "BYTE", 1, 0, "REAL", "")
			SaveVarPos = SaveVarPos->Next
		Loop
		If ChipFamily = 14 Or ChipFamily = 15 Then
			'Will need to put SysW, SysSTATUS into shared bank
			CurrLine = LinkedListInsert(CurrLine, ";Save Context")
			CurrLine = LinkedListInsert(CurrLine, " movwf SysW")
			CurrLine = LinkedListInsert(CurrLine, " swapf STATUS,W")
			CurrLine = LinkedListInsert(CurrLine, " movwf SysSTATUS")
			If ChipFamily = 15 Then
				AddVar "SysBSR", "BYTE", 1, 0, "REAL", ""
				CurrLine = LinkedListInsert(CurrLine, " movf BSR,W")
				CurrLine = LinkedListInsert(CurrLine, " banksel STATUS")
				CurrLine = LinkedListInsert(CurrLine, " movwf SysBSR")
			Else
				CurrLine = LinkedListInsert(CurrLine, " banksel STATUS")
			End If
			CurrLine = LinkedListInsert(CurrLine, " incf SysIntOffCount,F")
			CurrLine = LinkedListInsert(CurrLine, ";Store system variables")
			SaveVarPos = SaveVars->Next
			Do While SaveVarPos <> 0
				CurrLine = LinkedListInsert(CurrLine, " movf " + SaveVarPos->Value + ",W")
				CurrLine = LinkedListInsert(CurrLine, " movwf Save" + SaveVarPos->Value)
				SaveVarPos = SaveVarPos->Next
			Loop
			If PCHUsed Then
				CurrLine = LinkedListInsert(CurrLine, " clrf PCLATH")
			End If
		ElseIf ChipFamily = 16 Then
			AddVar "SysBSR", "BYTE", 1, 0, "REAL", ""
			CurrLine = LinkedListInsert(CurrLine, ";Save Context")
			CurrLine = LinkedListInsert(CurrLine, " movff WREG,SysW")
			CurrLine = LinkedListInsert(CurrLine, " movff STATUS,SysSTATUS")
			CurrLine = LinkedListInsert(CurrLine, " movff BSR,SysBSR")
			CurrLine = LinkedListInsert(CurrLine, " incf SysIntOffCount,F")
			CurrLine = LinkedListInsert(CurrLine, ";Store system variables")
			SaveVarPos = SaveVars->Next
			Do While SaveVarPos <> 0
				CurrLine = LinkedListInsert(CurrLine, " movff " + SaveVarPos->Value + ",Save" + SaveVarPos->Value)
				SaveVarPos = SaveVarPos->Next
			Loop
		End If
		
		'Add On Interrupt generated code
		CurrLine = LinkedListInsert(CurrLine, ";On Interrupt handlers")
		CurrLine = LinkedListInsertList(CurrLine, IntHandlerCode->CodeList)
		CurrLine = LinkedListInsert(CurrLine, ";User Interrupt routine")
		
		'Get last line in routine
		Do While CurrLine <> 0
			SubEnd = CurrLine
			CurrLine = CurrLine->Next
		Loop
		
		'Add context restore code
		CurrLine = SubEnd
		CurrLine = LinkedListInsert(CurrLine, "INTERRUPTDONE")
		CurrLine = LinkedListInsert(CurrLine, ";Restore Context")
		If ChipFamily = 14 Or ChipFamily = 15 Then
			CurrLine = LinkedListInsert(CurrLine, ";Restore system variables")
			SaveVarPos = SaveVars->Next
			Do While SaveVarPos <> 0
				CurrLine = LinkedListInsert(CurrLine, " movf Save" + SaveVarPos->Value + ",W")
				CurrLine = LinkedListInsert(CurrLine, " movwf " + SaveVarPos->Value)
				SaveVarPos = SaveVarPos->Next
			Loop
			CurrLine = LinkedListInsert(CurrLine, " clrf SysIntOffCount")
			If ChipFamily = 15 Then
				AddVar "SysBSR", "BYTE", 1, 0, "REAL", ""
				CurrLine = LinkedListInsert(CurrLine, " movf SysBSR,W")
				CurrLine = LinkedListInsert(CurrLine, " movwf BSR")
			End If
			CurrLine = LinkedListInsert(CurrLine, " swapf SysSTATUS,W")
			CurrLine = LinkedListInsert(CurrLine, " movwf STATUS")
			CurrLine = LinkedListInsert(CurrLine, " swapf SysW,F")
			CurrLine = LinkedListInsert(CurrLine, " swapf SysW,W")
			CurrLine = LinkedListInsert(CurrLine, " retfie")
		ElseIf ChipFamily = 16 Then
			CurrLine = LinkedListInsert(CurrLine, ";Restore system variables")
			SaveVarPos = SaveVars->Next
			Do While SaveVarPos <> 0
				CurrLine = LinkedListInsert(CurrLine, " movff Save" + SaveVarPos->Value + "," + SaveVarPos->Value)
				SaveVarPos = SaveVarPos->Next
			Loop
			CurrLine = LinkedListInsert(CurrLine, " clrf SysIntOffCount")
			CurrLine = LinkedListInsert(CurrLine, " movff SysW,WREG")
			CurrLine = LinkedListInsert(CurrLine, " movff SysSTATUS,STATUS")
			CurrLine = LinkedListInsert(CurrLine, " movff SysBSR,BSR")
			CurrLine = LinkedListInsert(CurrLine, " retfie 0")
		End If
		
	ElseIf ModeAVR Then
		'On AVR, need to add subs for context save/restore, and set up vectors to
		'call correct handlers
		
		'Add variables
		AddVar("SaveSysValueCopy", "BYTE", 1, 0, "REAL", "")
		AddVar("SaveSREG", "BYTE", 1, 0, "REAL", "")
		AddVar("SysIntOffCount", "BYTE", 1, 0, "REAL", "")
		'Variables to store registers
		SaveVarPos = SaveVars->Next
		Do While SaveVarPos <> 0
			AddVar("Save" + SaveVarPos->Value, "BYTE", 1, 0, "REAL", "")
			SaveVarPos = SaveVarPos->Next
		Loop
		
		'Interrupt subs must go on first page
		'Create context save sub
		SBC += 1: Subroutine(SBC) = NewSubroutine("SysIntContextSave")
		'Set up flags (required, not compiled, needs first page, needs return)
		With *Subroutine(SBC)
			.Required = -1
			.Compiled = 0
			.FirstPage = -1
			.NoReturn = 0
			SubStart = .CodeStart
		End With
		CurrLine = SubStart
		'Add context save code
		CurrLine = LinkedListInsert(CurrLine, ";Store SysValueCopy")
		CurrLine = LinkedListInsert(CurrLine, " sts SaveSysValueCopy,SysValueCopy")
		CurrLine = LinkedListInsert(CurrLine, ";Store SREG")
		CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy,SREG")
		CurrLine = LinkedListInsert(CurrLine, " sts SaveSREG,SysValueCopy")
		'Store all registers
		CurrLine = LinkedListInsert(CurrLine, ";Store registers")
		SaveVarPos = SaveVars->Next
		Do While SaveVarPos <> 0
			CurrLine = LinkedListInsert(CurrLine, " sts Save" + SaveVarPos->Value + "," + SaveVarPos->Value)
			SaveVarPos = SaveVarPos->Next
		Loop
		CurrLine = LinkedListInsert(CurrLine, ";Prevent interrupt from being re-enabled")
		'CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
		'CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
		CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy,1")
		CurrLine = LinkedListInsert(CurrLine, " sts SysIntOffCount,SysValueCopy")
		'Compile the sub
		CompileSubroutine(Subroutine(SBC))
		
		'Create context restore sub
		SBC += 1: Subroutine(SBC) = NewSubroutine("SysIntContextRestore")
		'Set up flags (required, not compiled, needs first page, no return)
		With *Subroutine(SBC)
			.Required = -1
			.Compiled = 0
			.FirstPage = -1
			.NoReturn = -1
			SubStart = .CodeStart
		End With
		CurrLine = SubStart
		'Add context restore code
		CurrLine = LinkedListInsert(CurrLine, ";Allow interrupt to be re-enabled")
		CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
		CurrLine = LinkedListInsert(CurrLine, " sts SysIntOffCount,SysValueCopy")
		'Restore all registers
		CurrLine = LinkedListInsert(CurrLine, ";Restore registers")
		SaveVarPos = SaveVars->Next
		Do While SaveVarPos <> 0
			CurrLine = LinkedListInsert(CurrLine, " lds " + SaveVarPos->Value + ",Save" + SaveVarPos->Value)
			SaveVarPos = SaveVarPos->Next
		Loop
		CurrLine = LinkedListInsert(CurrLine, ";Restore SREG")
		CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy,SaveSREG")
		CurrLine = LinkedListInsert(CurrLine, " out SREG,SysValueCopy")
		CurrLine = LinkedListInsert(CurrLine, ";Restore SysValueCopy")
		CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy,SaveSysValueCopy")
		CurrLine = LinkedListInsert(CurrLine, " reti")
		'Compile the sub
		CompileSubroutine(Subroutine(SBC))
		
		'Create subs for On Interrupt events
		For CurrVect = 1 To IntCount
			With Interrupts(CurrVect)
				If .Handler <> "" Then
					SBC += 1: Subroutine(SBC) = NewSubroutine("Int" + UCase(.Vector))
					Subroutine(SBC)->Compiled = -1
					Subroutine(SBC)->FirstPage = -1
					Subroutine(SBC)->Required = -1
					Subroutine(SBC)->NoReturn = -1
					CurrLine = Subroutine(SBC)->CodeStart
					
					CurrLine = LinkedListInsert(CurrLine, " rcall SysIntContextSave")
					CurrLine = LinkedListInsert(CurrLine, " rcall " + .Handler)
					If .FlagBit <> "" Then
						CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(.FlagBit, "0", ""))
					End If
					CurrLine = LinkedListInsert(CurrLine," rjmp SysIntContextRestore")
				End If
			End With
		Next
		
	ElseIf ModeZ8 Then
		'Z8 can use vectors, so same as AVR
		
	End If	
	
	'Delete save list
	LinkedListDeleteList(SaveVars, 0)
	
End Sub

Sub AddSysVarBits (CompSub As SubType Pointer)
	Dim As String TempData, BitName
	Dim As Integer PD, T, SV
	
	Dim As LinkedListElement Pointer CurrLine
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		TempData = CurrLine->Value
		IF INSTR(TempData, ".") <> 0 THEN GOTO AddNextLineBits
		IF Left(TempData, 1) = " " THEN GOTO AddNextLineBits
		
		'Only use for some commands
		T = 0
		IF Left(TempData, 4) = "SET " THEN T = 1
		IF Left(TempData, 4) = "DIR " THEN T = 1
		IF Left(TempData, 3) = "IF " THEN T = 1
		IF Left(TempData, 3) = "DO " THEN T = 1
		IF Left(TempData, 5) = "LOOP " THEN T = 1
		IF INSTR(TempData, "=") <> 0 THEN T = 1
		IF Left(TempData, 11) = "WAIT UNTIL " THEN T = 1
		IF Left(TempData, 11) = "WAIT WHILE " THEN T = 1
		IF T = 0 THEN GOTO AddNextLineBits
		
		FOR SV = 1 TO SVBC
			BitName = SysVarBits(SV).Name
			IF WholeINSTR(UCase(TempData), UCase(BitName)) = 2 THEN
				'PRINT DataSource
				IF UCase(BitName) <> "DIR" AND Left(TempData, 4) <> "DIR " THEN
					Replace CurrLine->Value, BitName, SysVarBits(SV).Parent + "." + BitName
					GOTO AddNextLineBits
				END IF
			END IF
		NEXT
		
AddNextLineBits:
		CurrLine = CurrLine->Next
	Loop
	
END SUB

SUB BuildMemoryMap
	
	'Dimension arrays
	REDIM FreeMem (MemSize + 10) As Integer
	REDIM VarLoc (MemSize + 10) As Integer
	Dim As String TempData
	Dim As Integer PD, Range, Min, Max, L, T
	
	'Mark all locations as used
	For PD = 0 To MemSize + 10
		FreeMem(PD) = 1
	Next
	
	'Mark locations as free if specified by a range
	For Range = 1 to MRC
		TempData = MemRanges(Range)
		Min = VAL("&h" + Left(TempData, INSTR(TempData, ":") - 1))
		Max = VAL("&h" + Mid(TempData, INSTR(TempData, ":") + 1))
		For L = Min To Max
			FreeMem(L) = 0
		Next
	Next
	
	'Produce list of free memory locations
	T = 0
	For PD = 1 To MemSize
		If FreeMem(PD) = 0 Then
			T += 1
			VarLoc(T) = PD
			If T = ChipRam Then Exit For
		End If
	Next
	FreeRAM = T
	
End SUB

SUB CalcConfig
	
	'No config registers on AVR that can be set by GCBASIC
	If ModeAVR Then Exit Sub
	
	Dim As String CurrItem, CurrName, CurrVal, Temp
	Dim As Integer CurrWord, CurrConfConst, CurrSpeed
	Dim As LinkedListElement Pointer UserSettingList, UserSettingLoc, CodeLoc
	Dim As LinkedListElement Pointer CurrSettingLoc, CurrSettingOptLoc
	Dim As ConfigSetting Pointer CurrSetting
	
	'Read config
	Do WHILE INSTR(CONFIG, "&") <> 0: Replace CONFIG, "&", ",": Loop
	'Split line into elements, put into list
	UserSettingList = LinkedListCreate
	UserSettingLoc = UserSettingList
	Do While InStr(Config, ",") <> 0
		UserSettingLoc = LinkedListInsert(UserSettingLoc, Trim(Left(Config, InStr(Config, ",") - 1)))
		Config = Trim(Mid(Config, InStr(Config, ",") + 1))
	Loop
	If Config <> "" Then
		UserSettingLoc = LinkedListInsert(UserSettingLoc, Config)
	End If
	
	'Search through list of user entered config settings
	'Then find a match in the list of valid settings for the PIC
	'If no match found, show error
	
	'Get a user setting
	UserSettingLoc = UserSettingList->Next
	Do While UserSettingLoc <> 0
		'Get a setting name
		CurrSettingLoc = ConfigSettings->Next
		Do While CurrSettingLoc <> 0
			CurrSetting = CurrSettingLoc->MetaData
			With (*CurrSetting)
				
				'Check for setting name in user setting
				If ConfigNameMatch(UserSettingLoc->Value, .Name) Then
					'Print "Name match for " + .Name
					'If found, find matching setting value
					CurrSettingOptLoc = .Options
					Do While CurrSettingOptLoc <> 0
						If ConfigValueMatch(CurrSettingOptLoc->Value, UserSettingLoc->Value) <> 0 Then
							'Matching setting value found
							'Print "Found match for: "; UserSettingLoc->Value; ", "; .Name; " = "; CurrSettingOptLoc->Value  
							.Setting = CurrSettingOptLoc
							If ConfigValueMatch(CurrSettingOptLoc->Value, UserSettingLoc->Value) = -2 Then GoTo ProcessNextSetting
						End If
						CurrSettingOptLoc = CurrSettingOptLoc->Next
					Loop
					
					If .Setting <> 0 Then GoTo ProcessNextSetting
				End If
				
			End With
			CurrSettingLoc = CurrSettingLoc->Next
		Loop
		
		'If setting has been found, will jump to ProcessNextSetting
		'Only way to get here is if a setting hasn't been found
		'Print "Bad setting: " + UserSettingLoc->Value
		Temp = Message("BadConfig")
		Replace Temp, "%option%", UserSettingLoc->Value
		LogWarning(Temp, "")
		
		ProcessNextSetting:
		UserSettingLoc = UserSettingLoc->Next
	Loop
	
	'Add default options
	'Find settings with nothing specified
	Dim As String DesiredSetting
	CurrSettingLoc = ConfigSettings->Next
	Do While CurrSettingLoc <> 0
		CurrSetting = CurrSettingLoc->MetaData
		With (*CurrSetting)
			If .Setting = 0 Then
				
				'Get the desired default setting
				DesiredSetting = ""
				If ConfigNameMatch(.Name, "MCLR") Then
					DesiredSetting = "OFF"
				ElseIf ConfigNameMatch(.Name, "LVP") Then
					DesiredSetting = "OFF"
				ElseIf ConfigNameMatch(.Name, "WDT") Then
					DesiredSetting = "OFF"
				ElseIf ConfigNameMatch(.Name, "PLLEN") Then
					DesiredSetting = "OFF"
				ElseIf ConfigNameMatch(.Name, "CPUDIV") Then
					DesiredSetting = "NOCLKDIV"
					
				ElseIf ConfigNameMatch(.Name, "OSC") Then
					'Get setting from #osc directive
					IF OSCType <> "" Then
						DesiredSetting = OSCType
						
					Else
						'No oscillator chosen, need to find best option
						'If ChipMhz = IntOscSpeed(x), use Int Osc
						'If ChipMhz > 4, use HS
						'If 4 > ChipMhz > ChipIntOsc, use XT
						
						'Check for internal osc
						If IntOscSpeeds <> 0 Then
							For CurrSpeed = 1 To IntOscSpeeds
								If ChipMhz = IntOscSpeed(CurrSpeed) Then
									DesiredSetting = "INT"
									Exit For
								End If
							Next
						End If
						
						'Can't use int osc, come up with suitable ext osc
						If DesiredSetting = "" Then
							If ChipMhz > 4 Then
								DesiredSetting = "HS"
							Else
								DesiredSetting = "XT"
							End If
						End If
					End If
					
				End If
				
				'If there is a default, use it
				If DesiredSetting <> "" Then
					CurrSettingOptLoc = .Options
					Do While CurrSettingOptLoc <> 0
						If ConfigValueMatch(CurrSettingOptLoc->Value, DesiredSetting) Then
							'Matching setting value found
							.Setting = CurrSettingOptLoc
							If ConfigValueMatch(CurrSettingOptLoc->Value, DesiredSetting) = -2 Then Exit Do
						End If
						CurrSettingOptLoc = CurrSettingOptLoc->Next
					Loop
				End If
				
			End If
		End With
		CurrSettingLoc = CurrSettingLoc->Next
	Loop
	
	'Store config
	'PIC 10/12/16 format
	If ChipFamily <> 16 Then
		
		'Find all settings
		CurrSettingLoc = ConfigSettings->Next
		Do While CurrSettingLoc <> 0	
			CurrSetting = CurrSettingLoc->MetaData	
			With (*CurrSetting)
				'If a setting has been set, need to write out
				If .Setting <> 0 Then
					
					'Find where setting came from
					'Print .Name, .Setting->Value
					For CurrConfConst = 1 To COC
						If ConfigNameMatch(ConfigOps(CurrConfConst).OP, .Name) And _
							ConfigValueMatch(ConfigOps(CurrConfConst).OP, .Setting->Value) Then
							'Store setting
							CurrWord = ConfigOps(CurrConfConst).Loc
							If OutConfig(CurrWord) = "" Then
								OutConfig(CurrWord) = "_" + ConfigOps(CurrConfConst).Op
							Else
								OutConfig(CurrWord) = OutConfig(CurrWord) + " & _" + ConfigOps(CurrConfConst).Op
							End If
							Exit For
						End If
					Next
					
				End If
			End With
			CurrSettingLoc = CurrSettingLoc->Next
		Loop
		
		'Write code
		'Single config word
		If ConfWords = 1 Then
			LinkedListInsert(ChipConfigCode->CodeList, " __CONFIG " + OutConfig(1))
		
		'Multiple config words
		Else
			CodeLoc = ChipConfigCode->CodeList
			For CurrWord = 1 To ConfWords
				'Only write non-blank words
				If OutConfig(CurrWord) <> "" Then 
					CodeLoc = LinkedListInsert(CodeLoc, " __CONFIG _CONFIG" + Str(CurrWord) + ", " + OutConfig(CurrWord))
				End If
			Next
		End If
		
	'PIC 18 format
	Else
		OutConfig(1) = ""
		
		'Use OSC or FOSC?
		Dim As Integer FOSC
		FOSC = 0
		For CurrConfConst = 1 To COC
			With ConfigOps(CurrConfConst)
				If Left(.Op, 5) = "FOSC_" Then
					FOSC = -1
					Exit For
				End If
			End With
		Next
		
		'Find all settings
		CurrSettingLoc = ConfigSettings->Next
		Do While CurrSettingLoc <> 0	
			CurrSetting = CurrSettingLoc->MetaData	
			With (*CurrSetting)
				'If a setting has been set, need to write out
				If .Setting <> 0 Then
					
					If FOSC Then
						If .Name = "OSC" Then .Name = "FOSC"
					End If
					
					If OutConfig(1) = "" Then
						OutConfig(1) = .Name + " = " + .Setting->Value
					Else
						OutConfig(1) = OutConfig(1) + ", " + .Name + " = " + .Setting->Value
					End If
				End If
			End With
			CurrSettingLoc = CurrSettingLoc->Next
		Loop
		
		LinkedListInsert(ChipConfigCode->CodeList, " CONFIG " + OutConfig(1))
		
	End If
	
END SUB

Sub CalcOps (OutList As CodeSection Pointer, SUM As String, AV As String, Ops As String, OriginIn As String)
	
	Dim As String Origin, Temp
	Dim As String Answer, ActN, AnswerIn, V1, V2, TypeV1, TypeV2, CalcType, Act
	Dim As Integer NeverLast, UnaryMode, SearchStart, CalcisBad, OpPos, SD, SO
	Dim As Integer CalcStart, CalcEnd, LastCalc, NextSame, PD
	
	Dim As LinkedListElement Pointer NewCode
	
	'PRINT "CalcOps origin", Origin, SUM
	Origin = OriginIn

	NeverLast = 0
	If UCase(Right(Origin, 2)) = "NL" Then
		NeverLast = -1
		Origin = Left(Origin, LEN(Origin) - 2)
	End If
	
	UnaryMode = 0
	If Left(Ops, 1) = "U" Then
		UnaryMode = -1
		Ops = Mid(Ops, 2)
	End If

	SearchStart = 1
SearchForOpAgain:
	'Initialise
	CalcIsBad = 0
	Answer = ""
	
	'Find operator
	OpPos = 0
	Act = ""
	FOR SD = SearchStart to LEN(SUM)
		Temp = "": IF SD > 1 Then Temp = Mid(SUM, SD - 1, 1)
		FOR SO = 1 to LEN(Ops)
			If (Not UnaryMode And Mid(SUM, SD, 1) = Mid(Ops, SO, 1)) Or (UnaryMode And Mid(SUM, SD, 1) = Mid(Ops, SO, 1) And IsCalcDivider(Temp)) Then
				Act = Mid(Ops, SO, 1)
				OpPos = SD
				'Exit For
				GoTo FoundOp
			End If
		Next
	Next
	FoundOp:
	
	'If operator not found, exit sub
	If OpPos = 0 Then Exit Sub
	
	'Get normal name for action
	ActN = Act
	If ActN = "{" Then ActN = "<="
	If ActN = "}" Then ActN = ">="
	If ActN = "~" Then ActN = "<>"
	
	'Get left operand
	V1 = ""
	CalcStart = 0
	If OpPos = 1 Or UnaryMode Then
		'CalcStart = 1
		CalcStart = OpPos
	Else
		For SD = OpPos - 1 to 0 step -1
			If SD = 0 Then
				V1 = Left(SUM, OpPos - 1)
				CalcStart = 1
				Exit For
			End If
			If IsCalcDivider(Mid(SUM, SD, 1)) Then
				If Mid(SUM, SD, 1) <> "-" Or (Not IsCalcDivider(Mid(SUM, SD - 1, 1)) And SD > 1) Then
					V1 = Left(SUM, OpPos - 1)
					V1 = Mid(V1, SD + 1)
					CalcStart = SD + 1
					Exit For
				End If
			End If
		Next
	End If
	
	'Get right operand
	V2 = ""
	CalcEnd = OpPos
	For SD = OpPos + 2 to (LEN(SUM) + 1)
		If SD > LEN(SUM) Then
			V2 = Mid(SUM, OpPos + 1)
			CalcEnd = LEN(SUM)
			Exit For
		End If
		If IsCalcDivider(Mid(SUM, SD, 1)) Then
			V2 = Left(SUM, SD - 1)
			V2 = Mid(V2, OpPos + 1)
			CalcEnd = SD - 1
			Exit For
		End If
	Next
	'Color 14
	'Print SUM, V1, Act, V2
	'Color 7
	
	'Check syntax
	CalcIsBad = 0
	If V1 = "" And Not UnaryMode Then
		If Act = "-" Then
			'If act is -, may have a negative literal
			SearchStart = OpPos + 1
			GoTo SearchForOpAgain
		Else
			Temp = Message("MissingOperand")
			Replace Temp, "%pos%", "before"
			Replace Temp, "%operator%", ActN
			LogError Temp, Origin
			CalcIsBad = -1
		End If
	End If
	If V2 = "" Then
		Temp = Message("MissingOperand")
		Replace Temp, "%pos%", "after"
		Replace Temp, "%operator%", ActN
		LogError Temp, Origin
		'Print SUM, Origin, Temp
		CalcIsBad = -1
	End If
	
	'If calc is bad, quit and try next operator
	If CalcIsBad Then
		SUM = Left(SUM, CalcStart - 1) + "SynErr" + Mid(SUM, CalcEnd + 1)
		Goto SearchForOpAgain
	End If
	
	'Get input types
	TypeV1 = TypeOfValue(V1, Subroutine(GetSubID(Origin)))
	TypeV2 = TypeOfValue(V2, Subroutine(GetSubID(Origin)))
	If UnaryMode Then TypeV1 = ""
	CalcType = GetCalcType(TypeV1, Act, TypeV2)
	'Print V1, Act, V2, CalcType, AV
	
	'Decide output variable
	If CalcStart = 1 And CalcEnd = LEN(SUM) And AV <> "" And (Not NeverLast) Then
		AnswerIn = AV
		LastCalc = -1
	Else
		AnswerIn = GetCalcVar(CalcType)
		LastCalc = 0
	End If
	
	'Does next calc use same system vars for result?
	NextSame = 0
	If CountOccur(SUM, "';*/%=~<>{}") > 1 And CountOccur(Act, "';*/%=~<>{}") > 0 Then NextSame = -1
	If NeverLast Then NextSame = -1
	'Print SUM, Act, NextSame
	
	'Define all vars found as byte
	'AddVar will ignore any constants/system vars/vars already defined
	AddVar DelType(V1), "BYTE", 1, 0, "REAL", Origin
	AddVar DelType(V2), "BYTE", 1, 0, "REAL", Origin
	AddVar AnswerIn, "BYTE", 1, 0, "REAL", Origin
	
	Color 3
	
	'Generate code for unary ops
	If UnaryMode Then
		Answer = CompileCalcUnary(OutList, Act, V2, Origin, AnswerIn)
		SearchStart += 1
	
	'Generate code for binary ops
	Else
		If Act = "*" Or Act = "/" Or Act = "%" Then Answer = CompileCalcMult(OutList, V1, Act, V2, Origin, AnswerIn)
		If Act = "+" Or Act = "-" Then Answer = CompileCalcAdd(OutList, V1, Act, V2, Origin, AnswerIn)
		If Act = "=" Or Act = "~" Or Act = "<" Or Act = ">" Or Act = "{" Or Act = "}" Then Answer = CompileCalcCondition(OutList, V1, Act, V2, Origin, AnswerIn)
		If Act = "&" Or Act = "|" Or Act = "#" Then Answer = CompileCalcLogic(OutList, V1, Act, V2, Origin, AnswerIn)
		'Note 13/2/2012: This error should never appear. Here just in case something stupid has happened!
		If Act = "!" Then Print "Internal error: Unary operator ! not handled properly"
	End If
	'Print SUM, V1, Act, V2, AnswerIn, Answer
	'Print
	
	'Don't bother with NextSame if calc result in const
	If IsConst(Answer) Then NextSame = 0
	
	'Copy answer if needed
	If Answer <> AnswerIn And (LastCalc Or NextSame) Then
		NewCode = CompileVarSet(Answer, AnswerIn, Origin)
		OutList->CodeEnd = LinkedListInsertList(OutList->CodeEnd, NewCode)
		FreeCalcVar Answer
		Answer = AnswerIn
	End If
	
	Color 7
	
	'Release calc vars
	If Answer <> V1 Then FreeCalcVar V1
	If Answer <> V2 Then FreeCalcVar V2
	If Answer <> AnswerIn And Not LastCalc Then FreeCalcVar AnswerIn

	'Store answer in sum
	'Print SUM,
	SUM = Left(SUM, CalcStart - 1) + Answer + Mid(SUM, CalcEnd + 1)
	'Print Answer, SUM
	
	Goto SearchForOpAgain
	
END SUB

Function CalcLineSize(CurrLine As String, ThisSubPage As Integer, CallPos As Integer, GotoPos As Integer, NextCodeLine As String) As Integer
	'Calculates the size in words of an assembly code line
	'CallPos is the location of the call instruction in the instruction list
	
	Dim As Integer InstSize, InstIndex, PresPos, TempDataCount, CalledSub, CalledSubPage
	Dim As String CurrLineVal, TempData(20), NextCallTarget
	Dim As Integer RestorePage, NextCalledSub, NextCalledSubPage
	Dim As SubType Pointer GotoSub
	
	Dim As String ROMData, Temp
	Dim As Integer DataBlockSize, RSC, DWIC, SS
	
	CurrLineVal = CurrLine
	
	'Replace PRESERVE with whatever is being preserved, need to do this for string tables
	IF Left(CurrLineVal, 9) = "PRESERVE " THEN
		PresPos = VAL(Mid(CurrLineVal, 10))
		CurrLineVal = ";" + PreserveCode(PresPos)
		If Mid(CurrLineVal, 2, 1) = Chr(8) Then CurrLineVal = Mid(CurrLineVal, 3)
	END If
	
	InstSize = 0
	InstIndex = IsASM(CurrLineVal)
	If InstIndex = 0 Then
		'If line not recognised as instruction, check for directive
		If ModePIC Then
			'Bank selection directives
			If Left(CurrLineVal, 9) = " banksel " Then
				If ChipFamily = 14 Then
					If HasSFRBit("RP1") Then
						InstSize = 2
					Else
						InstSize = 1
					End If
				ElseIf ChipFamily = 15 Then
					InstSize = 1
				ElseIf ChipFamily = 16 Then
					InstSize = 2
				End If
			ElseIf Left(CurrLineVal, 10) = " bankisel " Then
				InstSize = 1
				
			ElseIf Left(CurrLineVal, 9) = " pagesel " Then
				If ChipFamily = 15 Then
					InstSize = 1
				Else
					InstSize = 2
				End If
				
			'Data embedding directives
			ElseIf Left(CurrLineVal, 4) = " dw " OR Left(CurrLineVal, 4) = " db " Or Left(CurrLineVal, 4) = " de " THEN
				'Print "Data: "; CurrLineVal;
				DataBlockSize = 1
				If (ChipFamily = 14 Or ChipFamily = 15) And Left(CurrLineVal, 4) = " de " Then DataBlockSize = 2
				ROMData = Trim(Mid(CurrLineVal, 4))
				InstSize = 0
				'DWC = 0
				RSC = 0
				
				DO WHILE INSTR(ROMData, Chr(34)) <> 0
					Temp = Mid(ROMData, INSTR(ROMData, Chr(34)) + 1)
					Temp = Chr(34) + Left(Temp, INSTR(Temp, Chr(34)))
					RSC = RSC + 1
					Replace ROMData, Temp, "%S" + Str(RSC) + "S"
					Temp = Mid(Temp, 2)
					Temp = Left(Temp, LEN(Temp) - 1)
					TempData(RSC) = Temp
				LOOP
				
				DWIC = 0
				DO WHILE INSTR(ROMData, ",") <> 0
					DWIC += 1: CheckTemp(DWIC) = Trim(Left(ROMData, INSTR(ROMData, ",") - 1))
					ROMData = Trim(Mid(ROMData, INSTR(ROMDATA, ",") + 1))
				LOOP
				IF ROMData <> "" Then
					DWIC += 1: CheckTemp(DWIC) = ROMData
				End If
				
				SS = 0
				InstSize = 0
				Do While SS < DWIC
					SS += 1
					
					Temp = CheckTemp(SS)
					IF Temp <> "" THEN
						'Temp contains integer
						IF IsConst(Temp) Then
							'Print "Int", MakeDec(Temp)
							InstSize += 2
							
						'Temp contains string
						Else
							IF INSTR(Temp, "%S") <> 0 THEN
								Temp = TempData(Val(Mid(Temp, INSTR(Temp, "%S") + 2)))
							END If
							InstSize += LEN(Temp)
						END If
						
					END IF
				Loop
				
				InstSize = (InstSize + 1) \ 2
				'Print " Size:"; InstSize
			End If
		
		ElseIf ModeAVR Then
			'Data embedding directives
			If Left(CurrLineVal, 4) = ".DB " THEN
				'Print "Data: "; CurrLineVal;
				ROMData = Trim(Mid(CurrLineVal, 4))
				InstSize = 0
				
				DO WHILE INSTR(ROMData, ",") <> 0
					InstSize += 1
					ROMData = Trim(Mid(ROMData, INSTR(ROMDATA, ",") + 1))
				LOOP
				IF ROMData <> "" Then
					InstSize += 1
				End If
				
				'Print " Size:"; InstSize
			End If
		
		End If
		
	Else
		InstSize = AsmCommands(InstIndex).Words
	End If
	
	Return InstSize
End Function

Sub CalcSubSize(CurrSub As SubType Pointer)
	
	Dim As Integer FinalSize, CallPos, GotoPos, ThisSubPage, RetPos
	Dim As LinkedListElement Pointer CurrLine, NextLine
	Dim As String NextLineValue
	
	'Get the position of call instruction, saves work later
	CallPos = IsASM("call")
	
	'Get the position of return instruction
	If ModePIC Then
		RetPos = IsASM("return")
		GotoPos = IsASM("goto")
	ElseIf ModeAVR Or ModeZ8 Then
		RetPos = IsASM("ret")
		GotoPos = -1
	EndIf
	
	'Add page selection commands to sub so that size is accurate
	'(Will remove and re-add all page selection commands)
	AddPageCommands(CurrSub)
	
	'Find the page this sub is intended for
	ThisSubPage = CurrSub->DestPage
	
	FinalSize = 0
	CurrLine = CurrSub->CodeStart->Next
	Do While CurrLine <> 0
		
		'Get next code line
		NextLine = CurrLine->Next
		If NextLine <> 0 Then
			Do While (Left(NextLine->Value, 1) = ";" Or Left(NextLine->Value, 8) = "PRESERVE") AND NextLine->Next <> 0
				NextLine = NextLine->Next
			Loop
		End If
		NextLineValue = ""
		If NextLine <> 0 Then
			NextLineValue = NextLine->Value
		End If
		
		FinalSize += CalcLineSize(CurrLine->Value, ThisSubPage, CallPos, GotoPos, NextLineValue)
		CurrLine = CurrLine->Next
	Loop
	
	'Add size of return
	If Not CurrSub->NoReturn Then
		FinalSize += AsmCommands(RetPos).Words
	End If
	
	CurrSub->HexSize = FinalSize
	
	If FinalSize > CurrSub->MaxHexSize Then
		CurrSub->MaxHexSize = FinalSize
	End If
	
End Sub

FUNCTION CastOrder (InType As String) As Integer
	Select Case UCase(Trim(InType))
		Case "BIT": Return 0
		Case "BYTE": Return 1
		Case "WORD": Return 2
		Case "INTEGER": Return 3
		Case "LONG": Return 4
		Case "FLOAT": Return 5
		Case "STRING": Return 6
		Case Else: Return -1
	End Select
END FUNCTION

Sub CheckConstName (ConstName As String, Origin As String)
	
	Dim As String TempData
	Dim As Integer T
	
	'Names which will cause trouble
	T = 0
	IF INSTR(ConstName, " ") <> 0 THEN T = 1
	IF INSTR(ConstName, ",") <> 0 THEN T = 1
	IF INSTR(ConstName, "(") <> 0 THEN T = 1
	IF INSTR(ConstName, ")") <> 0 THEN T = 1
	IF ConstName = "W" THEN T = 1
	IF ConstName = "F" THEN T = 1
	IF ConstName = "B" THEN T = 1
	IF ConstName = "C" THEN T = 1
	IF ConstName = "Z" THEN T = 1
	IF T = 1 THEN
		TempData = Message("BadConstName")
		Replace TempData, "%const%", ConstName
		LogError TempData, Origin
	END IF
	
	'Names which may cause trouble
	If Len(ConstName) = 1 And T = 0 Then
		TempData = Message("WarningShortName")
		Replace TempData, "%type%", "constant"
		Replace TempData, "%name%", ConstName
		LogWarning TempData, Origin
	END If
End Sub

Sub CheckClockSpeed
	
	'Check speed that has been selected for the system clock.
	'If it is 0 Mhz, need to set to highest possible
	'If it is too high, show a warning
	
	Dim As Integer CurrSpeed
	Dim As Double MaxSpeed
	Dim As String SpeedMessage
	
	If ChipMhz = 0 Then
		
		'Find highest int osc speed
		MaxSpeed = -1
		If IntOscSpeeds <> 0 Then
			For CurrSpeed = 1 To IntOscSpeeds
				If MaxSpeed < IntOscSpeed(CurrSpeed) Then
					MaxSpeed = IntOscSpeed(CurrSpeed)
				End If
			Next
		End If
		
		'No suitable int osc, use highest ext osc speed
		If MaxSpeed = -1 Then
			MaxSpeed = ChipMaxSpeed
		End If
		
		'Set speed
		ChipMHz = MaxSpeed
		SpeedMessage = Message("AutoClockSpeed")
		Replace SpeedMessage, "%speed%", Str(ChipMhz)
		IF VBS = 1 THEN PRINT SPC(5); SpeedMessage
		
	ElseIf ChipMhz > ChipMaxSpeed Then
		'Show warning if speed too high
		LogWarning(Message("WarningOverclocked"))
	End If
	
End Sub

Sub CompileProgram
	
	Dim As Integer CurrSub, CompileMore, IntLoc, CurrInc, SubLoc
	Dim As String Temp
	
	'Check every sub in program, compile those that need to be compiled
	'Need to check again once a sub has been compiled, because that sub may
	'require other subs
	
	'Request initialisation routine
	RequestSub(0, "InitSys")
	
	'Find Interrupt sub, if found mark as required and set UserInt flag
	IntLoc = LocationOfSub("Interrupt", "")
	UserInt = 0
	If IntLoc <> 0 Then
		Subroutine(IntLoc)->Required = -1
		UserInt = -1
	End If
	
	'Add code to prevent program continuing into subs
	AddMainEndCode
	
	'Need to find all DIM statements before anything else
	For CurrSub = 0 To SBC
		CompileDim Subroutine(CurrSub)
	Next
	
	'Main Loop
	If VBS <> 0 Then
		Print Spc(5); Message("CompSubs")
	End If
	Do
		CompileMore = 0
		For CurrSub = 0 To SBC
			With *Subroutine(CurrSub)
				If .Required And Not .Compiled Then
					CompileSubroutine(Subroutine(CurrSub))
					CompileMore = -1
				End If
			End With
		Next
		
		'Check for required initialisation subs
		FOR CurrInc = 1 TO ICC
			Temp = Trim(gcINC(CurrInc, 2))
			If Temp <> "" And Left(Temp, 1) <> ";" Then
				SubLoc = LocationOfSub(Temp, "")
				If SubLoc <> 0 Then
					Subroutine(SubLoc)->FirstPage = -1
					If Not Subroutine(SubLoc)->Required Then
						Subroutine(SubLoc)->Required = -1
						CompileMore = -1
					End If
				End If
			End If
		Next
		
	Loop While CompileMore
	
	'Compile data/string tables
	CompileTables
	
	'Add context save/restore and int handlers to Interrupt
	AddInterruptCode
	
	'Add initialise code to start of main
	AddMainInitCode
	
	'Add code for individual pin dir set on PIC 12 bit
	If ChipFamily = 12 Then
		FixSinglePinSet
	End If
	
End Sub

Sub CompileSubroutine(CompSub As SubType Pointer)
	
	If VBS = 1 Then
		Print Spc(10); CompSub->Name
	End If
	
	'CompileDim (CompSub)
	
	SplitLines (CompSub)
	CompileSubCalls (CompSub)
	
	'Compile DIMs again, in case any come through from macros
	CompileDim (CompSub)
	
	CompileFor (CompSub)
	ProcessArrays (CompSub)
	AddSysVarBits (CompSub)
	CompileReadTable (CompSub)
	CompilePot (CompSub)
	CompileDo (CompSub)
	CompileDir (CompSub)
	CompileWait (CompSub)
	CompileOn (CompSub)
	CompileSet (CompSub)
	CompileRotate (CompSub)
	CompileRepeat (CompSub)
	CompileSelect (CompSub)
	CompileIF (CompSub)
	CompileVars (CompSub)
	CompileExitSub (CompSub)
	CompileGoto (CompSub)
	
	'Recognise ASM
	FindAssembly(CompSub)
	
	'Replace SysPointerX pseudo variable
	If ModeAVR Then FixPointerOps (CompSub)
	
	'CompileIntOnOff (CompSub) Need to do this after all subs compiled
	
	CompSub->Compiled = -1
End Sub

Sub CompileCalc (SUM As String, AV As String, Origin As String, ByRef OutList As CodeSection Pointer = 0)
	
	Dim As String TempData, InBrackets, OutTemp
	Dim As Integer T, BL, BC
	
	If OutList = 0 Then
		OutList = NewCodeSection
	End If
	
	'Simplify --, +-
	DO WHILE INSTR(SUM, "--") <> 0: Replace SUM, "--", "+": Loop
	DO WHILE INSTR(SUM, "+-") <> 0: Replace SUM, "+-", "-": LOOP
	
	'Calculate brackets
	Do While INSTR(SUM, "(") <> 0
		'Get the sum in brackets
		TempData = Mid(SUM, INSTR(SUM, "("))
		BL = 1
		FOR T = 2 TO LEN(TempData)
			IF Mid(TempData, T, 1) = "(" THEN BL = BL + 1
			IF Mid(TempData, T, 1) = ")" THEN BL = BL - 1
			IF BL = 0 THEN TempData = Left(TempData, T): EXIT FOR
		NEXT
		
		'Detect if whole sum is in brackets, remove brackets and try again if so
		If Len(TempData) = Len(SUM) Then
			SUM = Mid(Sum, 2, Len(Sum) - 2)
		Else
		
			'Use recursion to calculate sum in brackets
			InBrackets = Mid(TempData, 2)
			InBrackets = Left(InBrackets, LEN(InBrackets) - 1)
			OutTemp = ""
			CompileCalc InBrackets, OutTemp, Origin + "NL", OutList
			If OutTemp = "" Then OutTemp = InBrackets
			Replace SUM, TempData, OutTemp
		End If
	Loop
	
	'Calculate unary operations
	CalcOps OutList, SUM, AV, "U!-", Origin
	
	'Calculate binary operations
	CalcOps OutList, SUM, AV, "*/%", Origin
	CalcOps OutList, SUM, AV, "+-", Origin
	CalcOps OutList, SUM, AV, "=~<>{}", Origin
	CalcOps OutList, SUM, AV, "&|!#", Origin
	
End Sub

FUNCTION CompileCalcAdd(OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String

	Dim OutVal As LongInt
	Dim As String V1Type, V2Type, CalcType, DestType, AV, R1, R2, Temp
	Dim As Integer SourceSub, DestSub
	Dim As String CurrV1, CurrV2
	Dim As String Cmd, Ovr, TempVar
	Dim As Integer CD, CurrVarByte, CheckCarry
	
	Dim As LinkedListElement Pointer CurrLine, NewCode
	CurrLine = OutList->CodeEnd
	
	'Calculate +, -. Replace sum with variable containing answer
	
	'Notes for new code:
	' - Only need to work with enough bytes to fill answer. If A = 257, B = 256,
	'   A - B = 1 (with byte ops). If high bytes don't match, it will be
	'	impossible to store result anyway. No point calculating more bytes!
	' - Remember references! (@variable)
	' - addlw/sublw not implemented on 12 bit core
	'Optimisations:
	' - A +/- 0 = A
	' - B +/- 0 = B
	' - If A = B, A - B = 0, A + B = A << 1
	' - A + 1/A - 1/B + 1/B - 1: use incf/decf/inc/dec?
	' - 0 +/- 0: store 0 directly in output
	
	'Remove casts from variable names
	'These should have already been read by CalcOps, and a suitable size temp
	'or output variable should have been set.
	V1 = DelType(V1): V2 = DelType(V2)
	
	'Get types
	SourceSub = GetSubID(Origin)
	If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
	V1Type = TypeOfValue(V1, Subroutine(SourceSub))
	V2Type = TypeOfValue(V2, Subroutine(SourceSub))
	DestType = TypeOfVar(Answer, Subroutine(DestSub))
	CalcType = DestType
	
	'Get output var
	AV = Answer
	'Print "Calculating " + AV + " = " + V1 + " " + Act + " " + V2
	
	'Check if both are constants
	IF IsConst(V1) AND IsConst(V2) Then
		If InStr(V1, "@") = 0 AND INSTR(V2, "@") = 0 Then
			OutVal = 0
			IF Act = "+" THEN OutVal = MakeDec(V1) + MakeDec(V2)
			IF Act = "-" THEN OutVal = MakeDec(V1) - MakeDec(V2)
			If CalcType = "BYTE" Then
				Do While OutVal < 0: OutVal += 256: Loop
				Do While OutVal > 255: OutVal -= 256: Loop
			ElseIf CalcType = "WORD" Then
				Do While OutVal < 0: OutVal += 65536: Loop
				Do While OutVal > 65535: OutVal -= 65536: Loop
			ElseIf CalcType = "INTEGER" Then
				Do While OutVal < -32768: OutVal += 65536: Loop
				Do While OutVal > 32767: OutVal -= 65536: Loop
			ElseIf CalcType = "LONG" Then
				Do While OutVal < 0: OutVal += 2^32: Loop
				Do While OutVal >= 2^32: OutVal -= 2^32: Loop
			End If
			AV = Str(OutVal)
			GOTO AddSubAnswer
		Else
			'CalcAtAsm(T), CAAC
			'@SYSASMCALCn
			'Need to put calculation for assembler into CalcAtAsm(n)
			'Then put "@SYSASMCALCn" into the the line. It will be replace when suitable.
			For CurrVarByte = 0 To GetTypeSize(CalcType) - 1
				CAAC += 1
				CalcAtAsm(CAAC) = GetByte("@" + V1 + Act + V2, CurrVarByte)
				NewCode = CompileVarSet("@SYSASMCALC" + Str(CAAC), "[byte]" + GetByte(AV, CurrVarByte), Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
			Next
			GoTo AddSubAnswer
		End If
	END IF
  
	'Optimisations
	'Increment/Decrement (byte only)
	If CalcType = "BYTE" Then
		If V2 = "1" And INSTR(V1, "@") = 0 Then
			If ModePIC Then
				If V1 <> AV Then
					If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " incf " + V1 + ",W")
					If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " decf " + V1 + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + AV)
				Else
					If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " incf " + V1 + ",F")
					If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " decf " + V1 + ",F")
				End If
			ElseIf ModeAVR Then
				NewCode = LinkedListCreate
				R1 = PutInRegister(NewCode, V1, "BYTE", Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " inc " + R1)
				If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " dec " + R1)
				AV = R1
			End If
			Goto AddSubAnswer
		End If
		If V1 = "1" And Act = "+" And INSTR(V2, "@") = 0 Then
			If ModePIC Then
				If V2 <> AV Then
					CurrLine = LinkedListInsert(CurrLine, " incf " + V2 + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + AV)
				Else
					CurrLine = LinkedListInsert(CurrLine, " incf " + V2 + ",F")
				End If
			ElseIf ModeAVR Then
				NewCode = LinkedListCreate
				R2 = PutInRegister(NewCode, R2, "BYTE", Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				CurrLine = LinkedListInsert(CurrLine, " inc " + R2)
				AV = R2
			End If
			Goto AddSubAnswer
		End If
	End If
	
	If V1 = V2 And INSTR(V1, "@") = 0 Then
		If Act = "-" Then
			AV = "0"
			Goto AddSubAnswer
		End If
	End If
	
	'Swap V1, V2 if mode is add and V1 is const
	If Act = "+" And IsConst(V1) Then
		'Swap names
		Swap V1, V2
		
		'Swap Types
		Swap V1Type, V2Type
	End If
	
	'Snippet of old code, can sublw be used below to improve generated code?
	'		If ChipFamily <> 12 And IsConst(V1) Then
	'			Cmd = " addlw ": IF Act = "-" THEN Cmd = " sublw "
	'			CurrLine = LinkedListInsert(CurrLine, Cmd + GetByte(V1, 0))
	'			CurrLine = LinkedListInsert(CurrLine, " movwf " + AV)
	'			
	
	'Flag to indicate if carry/borrow check is needed
	CheckCarry = 0
	
	'Deal with each byte
	For CurrVarByte = 0 To GetTypeSize(CalcType) - 1
		
		'Get current byte of V1 and V2 (if they exist) or 0
		If GetTypeSize(V1Type) > CurrVarByte Then
			CurrV1 = GetByte(V1, CurrVarByte)
		Else
			CurrV1 = "0"
		End If
		If GetTypeSize(V2Type) > CurrVarByte Then
			CurrV2 = GetByte(V2, CurrVarByte)
		Else
			CurrV2 = "0"
		End If
		
		'Shortcut for multiple byte increment
		'(Especially useful for pointers and For loops)
		If V1 = AV And V2 = "1" And INSTR(V1, "@") = 0 And Act = "+" Then
			If ModePIC Then
				If CurrVarByte > 0 Then
					CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
				End If
				CurrLine = LinkedListInsert(CurrLine, " incf " + CurrV1 + ",F")
				
			ElseIf ModeAVR Then
				NewCode = LinkedListCreate
				R1 = PutInRegister(NewCode, CurrV1, "BYTE", Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				If CurrVarByte > 0 Then
					CurrLine = LinkedListInsert(CurrLine, " brne PC + 2")
				End If
				CurrLine = LinkedListInsert(CurrLine, " inc " + R1)
				'Copy result to output variable
				NewCode = CompileVarSet("[byte]" + R1, "[byte]" + GetByte(AV, CurrVarByte), Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				FreeCalcVar R1
			End If
			Goto AddSubNextByte
			
		End If
		
		'Is a calculation needed?
		'If both inputs 0, output 0
		'If one input 0, copy or negate (0 - x) other input
		'If both inputs non 0, then calculate. Check carry if needed (CurrVarByte > 0 and potential overflow)
		
		'Both inputs 0, output 0
		If CurrV1 = "0" And CurrV2 = "0" Then
			'Need carry?
			If CheckCarry Then
				'Carry won't be needed next time
				CheckCarry = 0
				
				If ModePIC Then
					'0 + C > F, no need to update C
					CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(AV, CurrVarByte))
					If Act = "+" Then
						CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,C")
						CurrLine = LinkedListInsert(CurrLine, " incf " + GetByte(AV, CurrVarByte) + ",F")
					Else
						CurrLine = LinkedListInsert(CurrLine, " btfss STATUS,C")
						'CurrLine = LinkedListInsert(CurrLine, " addlw 255")
						CurrLine = LinkedListInsert(CurrLine, " decf " + GetByte(AV, CurrVarByte) + ",F")
					End If
					'CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
					GoTo AddSubNextByte
					
				ElseIf ModeAVR Then
					R1 = GetCalcVar("BYTE")
					CurrLine = LinkedListInsert(CurrLine, " clr " + R1)
					CurrLine = LinkedListInsert(CurrLine, " brcc PC + 2")
					If Act = "+" Then
						CurrLine = LinkedListInsert(CurrLine, " inc " + R1)
					Else
						CurrLine = LinkedListInsert(CurrLine, " dec " + R1)
					End If
					NewCode = CompileVarSet(R1, "[BYTE]" + GetByte(AV, CurrVarByte), Origin)
					CurrLine = LinkedListInsertList(CurrLine, NewCode)
					FreeCalcVar R1
					GoTo AddSubNextByte
					
				End If
				
			Else
				'No high bytes, no carry, so put 0 into output byte
				NewCode = CompileVarSet("0", "[BYTE]" + GetByte(AV, CurrVarByte), Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				GoTo AddSubNextByte
			End If
			
			'Large chunk of troublesome optimisation code deleted here 8/2/2013
			
		End If
		
		'Need to calculate AV
		If ModePIC Then
			'On 16F1/18F, if V1 is const, put it into variable so subwfb can be used
			'On 12 bit core, V1 cannot be a constant due to lack of sublw instruction
			'If V1 is a constant, it needs to be put into a variable
			If IsConst(CurrV1) And (ChipFamily = 12 Or ((ChipFamily = 15 Or ChipFamily = 16) And CheckCarry)) Then
				CurrLine = LinkedListInsert(CurrLine, " movlw " + CurrV1)
				CurrV1 = GetCalcVar("BYTE")
				CurrLine = LinkedListInsert(CurrLine, " movwf " + CurrV1)
			END If
			
			'Put V2 into W
			Temp = " movf " + CurrV2 + ",W"
			IF IsConst(CurrV2) THEN Temp = " movlw " + CurrV2
			CurrLine = LinkedListInsert(CurrLine, Temp)
			
			'Add carry from low byte calc to W
			If CheckCarry And ChipFamily <> 15 And ChipFamily <> 16 Then
				Temp = " btfsc ": IF Act = "-" THEN Temp = " btfss "
				If ChipFamily = 12 Then
					TempVar = GetCalcVar("BYTE")
					CurrLine = LinkedListInsert(CurrLine, " movlw 1")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + TempVar)
					CurrLine = LinkedListInsert(CurrLine, Temp + "STATUS,C")
					CurrLine = LinkedListInsert(CurrLine, " addwf " + TempVar + ",W")
					FreeCalcVar TempVar
				Else
					CurrLine = LinkedListInsert(CurrLine, Temp + "STATUS,C")
					CurrLine = LinkedListInsert(CurrLine, " addlw 1")
				End If
			End If
			
			'Add W to V1, store - C already added, or 18F and no C
			If (ChipFamily <> 15 And ChipFamily <> 16) Or Not CheckCarry Then
				If V1 <> AV Or IsConst(V1) Then
					IF Act = "+" THEN CurrLine = LinkedListInsert(CurrLine, " addwf " + CurrV1 + ",W")
					IF Act = "-" Then
						If IsConst(CurrV1) Then
							CurrLine = LinkedListInsert(CurrLine, " sublw " + CurrV1)
						Else
							CurrLine = LinkedListInsert(CurrLine, " subwf " + CurrV1 + ",W")
						End If
					End If
					CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
				Else
					IF Act = "+" THEN CurrLine = LinkedListInsert(CurrLine, " addwf " + CurrV1 + ",F")
					IF Act = "-" THEN CurrLine = LinkedListInsert(CurrLine, " subwf " + CurrV1 + ",F")
				End If
			'Add W to V1, store - 18F, carry needed
			ElseIf ChipFamily = 15 Or ChipFamily = 16 Then
				If AV <> V1 Then	
					IF Act = "+" THEN CurrLine = LinkedListInsert(CurrLine, " addwfc " + CurrV1 + ",W")
					IF Act = "-" THEN CurrLine = LinkedListInsert(CurrLine, " subwfb " + CurrV1 + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
				Else
					IF Act = "+" THEN CurrLine = LinkedListInsert(CurrLine, " addwfc " + CurrV1 + ",F")
					IF Act = "-" THEN CurrLine = LinkedListInsert(CurrLine, " subwfb " + CurrV1 + ",F")
				End If
			End If
			'Carry or borrow may now have occured, must check on next byte
			CheckCarry = -1
			
		ElseIf ModeAVR Then
			
			'Put parameters into registers
			'Overwrite V1?
			Ovr = "O:"
			If UCase(Left(V1, 7)) = "SYSTEMP" Then Ovr = ""
			If UCase(Left(V1, 11)) = "SYSCALCTEMP" Then Ovr = ""
			
			'Put V1 into register, keep name in R1
			NewCode = LinkedListCreate 
			R1 = PutInRegister(NewCode, Ovr + CurrV1, "BYTE", Origin)
			CurrLine = LinkedListInsertList(CurrLine, NewCode)
			'Put V2 into register, keep name in R2
			NewCode = LinkedListCreate
			R2 = PutInRegister(NewCode, CurrV2, "BYTE", Origin)
			CurrLine = LinkedListInsertList(CurrLine, NewCode)
			If CurrVarByte = 0 Then
				AddVar V1, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
				AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
			End If
			
			If CheckCarry Then
				If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " adc " + R1 + "," + R2)
				If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " sbc " + R1 + "," + R2)
			Else
				If Act = "+" Then CurrLine = LinkedListInsert(CurrLine, " add " + R1 + "," + R2)
				If Act = "-" Then CurrLine = LinkedListInsert(CurrLine, " sub " + R1 + "," + R2)
			End If
			
			'Copy result to output variable
			NewCode = CompileVarSet("[byte]" + R1, "[byte]" + GetByte(AV, CurrVarByte), Origin)
			CurrLine = LinkedListInsertList(CurrLine, NewCode)
			FreeCalcVar R2
			FreeCalcVar R1
			'Carry or borrow may now have occured, must check on next byte
			CheckCarry = -1
		End If
		
		AddSubNextByte:
		
	Next
	
	'Replace sum with answer variable
AddSubAnswer:
	OutList->CodeEnd = CurrLine
	Return AV

END FUNCTION

FUNCTION CompileCalcCondition(OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
	
	Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2, AVH, CT1, CT2, SNT
	Dim As Integer SourceSub, DestSub
	Dim As String Cmd, CalcVarType
	Dim As Integer OutVal, PD
	Dim As LinkedListElement Pointer CurrLine
	CurrLine = OutList->CodeEnd
	
	'Calculate <,>,=,~. Replace sum with variable containing answer
	
	'Unusual symbols:
	' ~ (not equal)
	' } (equal or greater)
	' { (less or equal)

	'Check if both are constants
	IF IsConst(V1) AND IsConst(V2) And INSTR(V1, "@") = 0 AND INSTR(V2, "@") = 0 THEN
		OutVal = 0
		IF Act = "=" THEN OutVal = MakeDec(V1) = MakeDec(V2)
		IF Act = "~" THEN OutVal = MakeDec(V1) <> MakeDec(V2)
		IF Act = "<" THEN OutVal = MakeDec(V1) < MakeDec(V2)
		IF Act = ">" THEN OutVal = MakeDec(V1) > MakeDec(V2)
		IF Act = "{" THEN OutVal = MakeDec(V1) <= MakeDec(V2)
		IF Act = "}" THEN OutVal = MakeDec(V1) >= MakeDec(V2)
		If OutVal Then OutVal = 255 Else OutVal = 0
		Return Str(OutVal)
	END IF
	
	'Get types
	SourceSub = GetSubID(Origin)
	If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
	V1Type = TypeOfValue(V1, Subroutine(SourceSub))
	V2Type = TypeOfValue(V2, Subroutine(SourceSub))
	If CastOrder(V1Type) > CastOrder(V2Type) Then
		CalcType = V1Type
	Else
		CalcType = V2Type
	End If
	DestType = "BYTE" 'Remove any doubt that may exist! (unlikely)
	V1 = DelType(V1): V2 = DelType(V2)
	V1O = V1: V2O = V2
	
	'Generate asm code for sum
	
	'Special shortcut code for bit test
	If CalcType = "BIT" And IsConst(V2) Then
		
		'Translate less/more into equal/not equal
		If Act = "<" Or Act = ">" Then Act = "~"
		If Act = "{" Or Act = "}" Then Act = "="
		'Decide test status
		Dim As Integer TestFor = 0
		If Act = "=" And MakeDec(V2) = 1 Then TestFor = 1
		If Act = "~" And MakeDec(V2) = 0 Then TestFor = 1
		
		'Get Bit name and number
		Dim As String BitName
		BitName = V1
		Replace BitName, ".", ","
		
		AddVar "SysByteTempX", "Byte", 1, Subroutine(SourceSub), "REAL", Origin
		If ModePIC Then
			CurrLine = LinkedListInsert(CurrLine, " clrf SysByteTempX")
			If TestFor = 1 Then
				CurrLine = LinkedListInsert(CurrLine, " btfsc " + BitName)
			Else
				CurrLine = LinkedListInsert(CurrLine, " btfss " + BitName)
			End If
			CurrLine = LinkedListInsert(CurrLine, " comf SysByteTempX,F")
			GoTo CompileConditionDone
		ElseIf ModeAVR Then
			
			
		ElseIf ModeZ8 Then
			
		End If
	End If
	
	'Copy parameters
	If CalcType = "STRING" Then
		'Copy pointers
		'SetStringPointers (V1, V2, Origin)
		'Print "String comparison ", V1, V1Type, V2, V2Type, Origin
		CurrLine = LinkedListInsertList(CurrLine, SetStringPointers(V1, V2, Subroutine(SourceSub), Origin))
	Else
		CalcVarType = CalcType
		If CalcVarType = "BIT" Then CalcVarType = "BYTE"
		'Declare SysCalcTempA, SysCalcTempB and SysCalcTempX
		AddVar "Sys" + CalcVarType + "TempA", CalcType, 1, Subroutine(SourceSub), "REAL", Origin
		AddVar "Sys" + CalcVarType + "TempB", CalcType, 1, Subroutine(SourceSub), "REAL", Origin
		AddVar "SysByteTempX", "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
		
		'Copy values
'		If Act = ">" Or Act = "}" Then
		If Act = ">" Or Act = "{" Then
			CT1 = "Sys" + CalcVarType + "TempB"
			CT2 = "Sys" + CalcVarType + "TempA"
		Else
			CT1 = "Sys" + CalcVarType + "TempA"
			CT2 = "Sys" + CalcVarType + "TempB"
		End If
		CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, CT1, Origin))
		CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, CT2, Origin))
	End If
	
	'Decide which sub to use
	IF Act = "=" Or Act = "~" THEN SNT = "SysCompEqual"
	'IF Act = "<" Or Act = ">" THEN SNT = "SysCompLessThan"
	'IF Act = "{" OR Act = "}" THEN SNT = "SysCompLessOrEqual"
	IF Act = "<" Or Act = ">" Or Act = "{" OR Act = "}" Then SNT = "SysCompLessThan"
	IF CalcType = "WORD" THEN SNT += "16"
	IF CalcType = "INTEGER" THEN
		If Act = "=" Or Act = "~" Then
			SNT += "16"
		Else
			SNT += "INT"
		End If
	End If
	If CalcType = "LONG" Then SNT += "32"
	IF CalcType = "SINGLE" THEN SNT += "Single"
	IF CalcType = "STRING" THEN SNT += "String"
	
	'Call calculation sub
	CurrLine = LinkedListInsert(CurrLine, " call " + SNT)
	RequestSub (Subroutine(SourceSub), SNT)
	
	'Invert answer for not equal
	'If Act = "~" Then
	If Act = "~" Or Act = "{" Or Act = "}" Then
		If ModePIC Then
			CurrLine = LinkedListInsert(CurrLine, " comf SysByteTempX,F")
		ElseIf ModeAVR Then
			CurrLine = LinkedListInsert(CurrLine, " com SysByteTempX")
		End If
	End If
	
	'Write answer
	CompileConditionDone:
	AV = "SysByteTempX"
	OutList->CodeEnd = CurrLine
	Return AV
	
END FUNCTION

FUNCTION CompileCalcLogic (OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
	
	Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2
	Dim As String CurrV1, CurrV2, CurrAct, Temp, Cmd, Ovr
	Dim As Integer CD, SourceSub, DestSub, CurrVarByte
	Dim As LinkedListElement Pointer CurrLine, NewCode
	CurrLine = OutList->CodeEnd
	
	'Delete casts (not needed for logic operations)
	V1 = DelType(V1): V2 = DelType(V2)
	
	'Get types
	'Print V1, Act, V2, AV
	SourceSub = GetSubID(Origin)
	If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
	V1Type = TypeOfValue(V1, Subroutine(SourceSub))
	V2Type = TypeOfValue(V2, Subroutine(SourceSub))
	DestType = TypeOfVar(Answer, Subroutine(DestSub))
	CalcType = DestType
	
	'Check types
	If V1Type = "SINGLE" Or V1Type = "STRING" Then
		Temp = Message("OperandTypeMismatch")
		Replace Temp, "%operand%", Act
		Replace Temp, "%type%", V1Type
		LogError Temp, Origin
	End If
	If V2Type = "SINGLE" Or V2Type = "STRING" Then
		Temp = Message("OperandTypeMismatch")
		Replace Temp, "%operand%", Act
		Replace Temp, "%type%", V2Type
		LogError Temp, Origin
	End If
	
	'Convert bit vars to byte
	If V1Type = "BIT" And Not IsConst(V1) Then
		V1Type = "BYTE"
		V1O = V1
		V1 = GetCalcVar("BYTE")
		CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1O, V1, Origin))
	End If
	If V2Type = "BIT" And Not IsConst(V2) Then
		V2Type = "BYTE"
		V2O = V2
		V2 = GetCalcVar("BYTE")
		CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2O, V2, Origin))
	End If
	
	'Get output var
	AV = Answer
	
	'If V1 and V2 are constant, produce constant result
	If (Act <> "!" And IsConst(V1) And IsConst(V2)) OR (Act = "!" And IsConst(V2)) Then
		IF Act = "&" THEN Return Trim(Str(MakeDec(V1) AND MakeDec(V2)))
		IF Act = "|" THEN Return Trim(Str(MakeDec(V1) OR MakeDec(V2)))
		IF Act = "#" THEN Return Trim(Str(MakeDec(V1) XOR MakeDec(V2)))
	End If

	'If V1 not const, V2 const, swap
	If IsConst(V2) Then
		Swap V1, V2
		Swap V1Type, V2Type
	End If
	
	'Perform operation on each pair of bytes
	For CurrVarByte = 0 To GetTypeSize(CalcType) - 1
		
		'Get current byte of V1 and V2 (if they exist) or 0
		If GetTypeSize(V1Type) > CurrVarByte Then
			CurrV1 = GetByte(V1, CurrVarByte)
		Else
			CurrV1 = "0"
		End If
		If GetTypeSize(V2Type) > CurrVarByte Then
			CurrV2 = GetByte(V2, CurrVarByte)
		Else
			CurrV2 = "0"
		End If
		CurrAct = Act
		
		'Optimisations for when one var is 255
		'Print "Before:", CurrV1, CurrAct, CurrV2
		If CurrV1 = "255" Then
			'255 & x = 0 | x
			If CurrAct = "&" Then
				CurrV1 = "0"
				CurrAct = "|"
			'255 | x = 0 | 255
			ElseIf CurrAct = "|" Then
				CurrV1 = "0"
				CurrV2 = "255"
			End If
		ElseIf CurrV2 = "255" Then
			'x & 255 = x | 0
			If CurrAct = "&" Then
				CurrV2 = "0"
				CurrAct = "|"
			'x | 255 = 0 | 255 (need V1 = 0 for below)
			ElseIf CurrAct = "|" Then
				CurrV1 = "0"
			End If
		End If
		
		'Should always try to have constant in V1 only
		If CurrV2 = "0" And Not IsConst(CurrV1) Then
			'Can swap order of each byte with no consequences
			'Don't need to swap types (VxType) either.
			Swap CurrV1, CurrV2
		End If
		'Print "After:", CurrV1, CurrAct, CurrV2
		
		'Possible inputs:
		'V1 = 0, V2 = 0 > AV = 0
		'V1 = 0, V2 = x > AV = V2 (OR/XOR) or 0 (AND)
		'V1 = x, V2 = x > AV = V1 op V2
		
		'First case, no V1 or V2
		If CurrV1 = "0" And CurrV2 = "0" Then
			'0 | 0 = 0, 0 & 0 = 0, 0 # 0 = 0
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(AV, CurrVarByte))
			ElseIf ModeAVR Then
				NewCode = CompileVarSet("0", "[byte]" + GetByte(AV, CurrVarByte), Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
			End If
			
		'Second case, V2 only
		ElseIf CurrV1 = "0" And CurrV2 <> "0" Then
			'Anything AND 0 = 0, so clear result
			If CurrAct = "&" Then
				If ModePIC Then 
					CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(AV, CurrVarByte))
				ElseIf ModeAVR Then
					'CurrLine = LinkedListInsert(CurrLine, " clr " + GetByte(AV, CurrVarByte))
					NewCode = CompileVarSet("0", "[byte]" + GetByte(AV, CurrVarByte), Origin)
					CurrLine = LinkedListInsertList(CurrLine, NewCode)
				End If
				
			'Anything OR/XOR 0 = Anything, so copy V2
			ElseIf CurrAct = "|" Or CurrAct = "#" Then
				NewCode = CompileVarSet("[byte]" + CurrV2, "[byte]" + GetByte(AV, CurrVarByte), Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
			End If
			
		'Third case, V1 and V2
		ElseIf CurrV1 <> "0" And CurrV2 <> "0" Then
			If ModePIC Then
				Cmd = " movf " + GetByte(V1, CurrVarByte) + ",W"
				If IsConst(V1) Then Cmd = " movlw " + GetByte(V1, CurrVarByte)
				CurrLine = LinkedListInsert(CurrLine, Cmd)
				IF CurrAct = "&" THEN Cmd = " andwf "
				IF CurrAct = "|" THEN Cmd = " iorwf "
				IF CurrAct = "#" THEN Cmd = " xorwf "
				If CurrVarByte = 0 Then
					AddVar V2, "BYTE", 1, 0, "REAL", Origin
				End If
				If V2 <> AV Then
					CurrLine = LinkedListInsert(CurrLine, Cmd + GetByte(V2, CurrVarByte) + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
				Else
					CurrLine = LinkedListInsert(CurrLine, Cmd + GetByte(V2, CurrVarByte) + ",F")
				End If
				
			ElseIf ModeAVR Then
				'If AVR, put values in registers
				NewCode = LinkedListCreate
				R1 = PutInRegister(NewCode, CurrV1, "BYTE", Origin)
				If CurrVarByte = 0 Then AddVar V1, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				'Overwrite V2?
				Ovr = "O:"
				If UCase(Left(V2, 7)) = "SYSTEMP" Then Ovr = ""
				If UCase(Left(V2, 11)) = "SYSCALCTEMP" Then Ovr = ""
				
				NewCode = LinkedListCreate
				R2 = PutInRegister(NewCode, Ovr + CurrV2, "BYTE", Origin)
				If CurrVarByte = 0 Then AddVar V2, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				
				IF CurrAct = "&" THEN CurrLine = LinkedListInsert(CurrLine, " and " + R2 + "," + R1)
				IF CurrAct = "|" THEN CurrLine = LinkedListInsert(CurrLine, " or " + R2 + "," + R1)
				IF CurrAct = "#" THEN CurrLine = LinkedListInsert(CurrLine, " eor " + R2 + "," + R1)
				
				'Copy result to output variable
				NewCode = CompileVarSet(R2, "[byte]" + GetByte(AV, CurrVarByte), Origin)
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				FreeCalcVar R2
				FreeCalcVar R1
			End If
			
		End If
		
	Next
	
	OutList->CodeEnd = CurrLine
	Return AV

END FUNCTION

FUNCTION CompileCalcMult (OutList As CodeSection Pointer, V1 As String, Act As String, V2 As String, Origin As String, Answer As String) As String
	
	Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2, AVH, SNT
	Dim As String Temp
	Dim As Integer SourceSub, DestSub, CD, PD
	Dim As LinkedListElement Pointer CurrLine, NewCode
	CurrLine = OutList->CodeEnd
	
	'Calculate *, /, %. Replace sum with variable containing answer
	
	'Get types
	SourceSub = GetSubID(Origin)
	If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
	V1Type = TypeOfValue(V1, Subroutine(SourceSub))
	V2Type = TypeOfValue(V2, Subroutine(SourceSub))
	DestType = TypeOfVar(Answer, Subroutine(DestSub))
	CalcType = DestType
	If CastOrder(V1Type) > CastOrder(CalcType) Then CalcType = V1Type
	If CastOrder(V2Type) > CastOrder(CalcType) Then CalcType = V2Type
	V1 = DelType(V1): V2 = DelType(V2)
	V1O = V1: V2O = V2
	
	'Get output var
	If DestType = CalcType Then
		AV = Answer
	Else
		AV = GetCalcVar(CalcType)
	End If
	
	'Generate asm code for sum
	'Check if both are constants
	IF IsConst(V1) AND IsConst(V2) And INSTR(V1, "@") = 0 AND INSTR(V2, "@") = 0 THEN
		IF Act = "*" THEN AV = Str(MakeDec(V1) * MakeDec(V2))
		IF Act = "/" THEN AV = Str(INT(MakeDec(V1) / MakeDec(V2)))
		IF Act = "%" THEN AV = Str(MakeDec(V1) MOD MakeDec(V2))
		GOTO MultDivAnswer
	END IF
	
	'Simplifications
	If Act = "*" And (V1 = "0" Or V2 = "0") Then AV = "0": Goto MultDivAnswer
	If Act = "*" And V1 = "1" Then AV = V2: Goto MultDivAnswer
	If (Act = "*" OR Act = "/") And V2 = "1" Then AV = V1: Goto MultDivAnswer
	If (Act = "/" Or Act = "%") And V1 = "0" Then AV = "0": Goto MultDivAnswer
	
	'Use hardware multiply instruction for byte*byte multiply
	If CalcType = "BYTE" And Act = "*" And ChipFamily = 16 Then
		If IsConst(V1) Then
			CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V1)))
		Else
			CurrLine = LinkedListInsert(CurrLine, " movf " + V1 + ",W")
		End If
		If IsConst(V2) Then
			CurrLine = LinkedListInsert(CurrLine, " mullw " + Str(MakeDec(V2)))
		Else
			CurrLine = LinkedListInsert(CurrLine, " mulwf " + V2)
		End If
		AV = "PRODL"
		Goto MultDivAnswer
	End If
	
	'Rotate left/right for mult or div by 2
	IF CalcType = "BYTE" And MakeDec(V2) = 2 Then
		
		AddVar V1, "BYTE", 1, 0, "REAL", Origin
		If ModeAVR Then
			Temp = ""
			If Act = "%" THEN Temp = "H:"
			R1 = PutInRegister(NewCode, "O:" + Temp + V1, CalcType, Origin)
			CurrLine = LinkedListInsertList(CurrLine, NewCode)
		End If
		
		If Act = "*" Then
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " bcf STATUS,C")
				If V1 = Answer Then
					CurrLine = LinkedListInsert(CurrLine, " rlf " + V1 + ",F")
				Else
					CurrLine = LinkedListInsert(CurrLine, " rlf " + V1 + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + Answer)
				End If
				AV = Answer
			ElseIf ModeAVR Then
				CurrLine = LinkedListInsert(CurrLine, " lsl " + R1)
				AV = R1
			End If
			
		ElseIf Act = "/" Then
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " bcf STATUS,C")
				If V1 = Answer Then
					CurrLine = LinkedListInsert(CurrLine, " rrf " + V1 + ",F")
				Else
					CurrLine = LinkedListInsert(CurrLine, " rrf " + V1 + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + Answer)
				End If
				AV = Answer
			ElseIf ModeAVR Then
				CurrLine = LinkedListInsert(CurrLine, " lsr " + R1)
				AV = R1
			End If
			
		ElseIf Act = "%" Then
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Answer)
				CurrLine = LinkedListInsert(CurrLine, " btfsc " + V1 + ",0")
				CurrLine = LinkedListInsert(CurrLine, " incf " + Answer + ",F")
				AV = Answer
			ElseIf ModeAVR Then
				CurrLine = LinkedListInsert(CurrLine, " andi " + R1 + ",1")
				AV = R1
			End If
		End If
		Goto MultDivAnswer
	End If
	
	'Declare variables needed for calculation
	AddVar "Sys" + CalcType + "TempA", CalcType, 1, 0, "REAL", Origin
	AddVar "Sys" + CalcType + "TempB", CalcType, 1, 0, "REAL", Origin
	AddVar "Sys" + CalcType + "TempX", CalcType, 1, 0, "REAL", Origin
	IF Act = "/" OR Act = "%" THEN AddVar "SysDivLoop", "BYTE", 1, 0, "REAL", Origin
	
	'Copy parameters
	CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, "Sys" + CalcType + "TempA", Origin))
	CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, "Sys" + CalcType + "TempB", Origin))
	
	'Use hardware multiply instruction for byte*byte multiply
	If CalcType = "BYTE" And Act = "*" And ModeAVR And HMult Then
		CurrLine = LinkedListInsert(CurrLine, " mul SysByteTempA,SysByteTempB")
		AV = "SysByteTempX"
		Goto MultDivAnswer
	End If
	
	'Decide which sub to call
	IF Act = "*" THEN SNT = "SysMultSub"
	IF Act = "/" THEN SNT = "SysDivSub"
	IF Act = "%" THEN SNT = "SysDivSub"
	IF CalcType = "WORD" THEN SNT += "16"
	IF CalcType = "INTEGER" THEN SNT += "INT"
	If CalcType = "LONG" Then SNT += "32"
	IF CalcType = "SINGLE" THEN SNT += "SINGLE"
	
	'Call calculation sub
	CurrLine = LinkedListInsert(CurrLine, " call " + SNT)
	RequestSub (Subroutine(SourceSub), SNT) 'Mark as used
	
	'Write answer
	If Act = "*" Then AV = "Sys" + CalcType + "TempX"
	If Act = "/" Then AV = "Sys" + CalcType + "TempA"
	If Act = "%" Then AV = "Sys" + CalcType + "TempX"
	'Print V1, V2, AV, CalcType
	
	'Replace sum with answer variable
MultDivAnswer:
	OutList->CodeEnd = CurrLine
	Return AV

END FUNCTION

Function CompileCalcUnary(OutList As CodeSection Pointer, Act As String, V2 As String, Origin As String, Answer As String) As String
	
	Dim As String V1O, V2O, V1Type, V2Type, CalcType, DestType, AV, R1, R2, AVH, SNT
	Dim As String Temp, Ovr
	Dim As Integer SourceSub, DestSub, CD, CurrVarByte
	Dim As LinkedListElement Pointer CurrLine, NewCode
	CurrLine = OutList->CodeEnd
	
	'Delete casts (not needed for logic operations)
	If Act = "!" Then V2 = DelType(V2)
	
	'Get types
	SourceSub = GetSubID(Origin)
	If INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = SourceSub
	V2Type = TypeOfValue(V2, Subroutine(SourceSub))
	DestType = TypeOfVar(Answer, Subroutine(DestSub))
	CalcType = DestType
	V2 = DelType(V2)
	
	'If op is negate, return an integer
	If Act = "-" And CastOrder(CalcType) < CastOrder("INTEGER") Then CalcType = "INTEGER"
	
	'Check types
	If V2Type = "STRING" Or (V2Type = "SINGLE" and Act = "!") Then
		Temp = Message("OperandTypeMismatch")
		Replace Temp, "%operand%", Act
		Replace Temp, "%type%", V2Type
		LogError Temp, Origin
	End If
	
	'Convert bit vars to byte
	If V2Type = "BIT" And Not IsConst(V2) Then
		V2Type = "BYTE"
		V2O = V2
		V2 = GetCalcVar("BYTE")
		CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2O, V2, Origin))
	End If
	
	'Get output var
	AV = Answer
	
	'If V2 is constant, produce constant result
	'Print "Unary: ", Act, V2, " > ", AV
	If IsConst(V2) And InStr(V2, "@") = 0 Then
		IF Act = "!" THEN
			If CalcType = "BYTE" Then Return Trim(Str((NOT MakeDec(V2)) AND 255))
			If CalcType = "WORD" OR CalcType = "INTEGER" Then Return Trim(Str((Not MakeDec(V2) And 65535)))
			If CalcType = "LONG" Then Return Trim(Str(NOT MakeDec(V2)))
		ElseIf Act = "-" THEN
			'Do nothing
			Return "-" + V2
		End If
	End If
	
	'Process each byte of variable one at a time
	For CurrVarByte = 0 To GetTypeSize(CalcType) - 1
		
		'NOT (Byte and Word)
		
		'No matching byte in input, so set output byte to 255
		If CurrVarByte >= GetTypeSize(V2Type) Then
			If ModePIC Then
				If ChipFamily <> 16 Then
					CurrLine = LinkedListInsert(CurrLine, " movlw 255")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
				End If
				If ChipFamily = 16 Then CurrLine = LinkedListInsert(CurrLine, " setf " + GetByte(AV, CurrVarByte))
			ElseIf ModeAVR Then
				'R2 = GetCalcVar("BYTE")
				'CurrLine = LinkedListInsert(CurrLine, " clr " + R2)
				'CurrLine = LinkedListInsert(CurrLine, " com " + R2)
				R2 = "255"
			End If
			
		'Matching byte in input for current output byte, so invert input
		Else
			If ModePIC Then
				If AV <> V2 Then
					CurrLine = LinkedListInsert(CurrLine, " comf " + GetByte(V2, CurrVarByte) + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(AV, CurrVarByte))
					If CurrVarByte = 0 Then
						AddVar V2, "BYTE", 1, 0, "REAL", Origin
						AddVar AV, "BYTE", 1, 0, "REAL", Origin
					End If
				Else
					CurrLine = LinkedListInsert(CurrLine, " comf " + GetByte(V2, CurrVarByte) + ",F")
					If CurrVarByte = 0 Then
						AddVar V2, "BYTE", 1, 0, "REAL", Origin
					End If
				End If
					
			ElseIf ModeAVR Then
				'If AVR, put current byte in register
				'Overwrite V2?
				Ovr = "O:"
				If UCase(Left(V2, 7)) = "SYSTEMP" Then Ovr = ""
				If UCase(Left(V2, 11)) = "SYSCALCTEMP" Then Ovr = ""
				
				NewCode = LinkedListCreate
				R2 = PutInRegister(NewCode, Ovr + GetByte(V2, CurrVarByte), "BYTE", Origin)
				AddVar V2, "BYTE", 1, 0, "REAL", Origin
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				
				'Perform operation
				CurrLine = LinkedListInsert(CurrLine, " com " + R2)
				
			End If
		End If
		
		If ModeAVR Then
			'Copy result to output variable
			NewCode = CompileVarSet(R2, "[byte]" + GetByte(AV, CurrVarByte), Origin)
			CurrLine = LinkedListInsertList(CurrLine, NewCode)
			FreeCalcVar R2
		End If
		
	Next
	
	If Act = "-" And CalcType = "INTEGER" Then
		If ModePIC Then
			CurrLine = LinkedListInsert(CurrLine, " incf " + GetByte(AV, 0) + ",F")
			CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
			CurrLine = LinkedListInsert(CurrLine, " incf " + GetByte(AV, 1) + ",F")
			
		ElseIf ModeAVR Then
			CurrLine = LinkedListInsert(CurrLine, " inc " + GetByte(AV, 0))
			CurrLine = LinkedListInsert(CurrLine, " sbic SREG,C")
			CurrLine = LinkedListInsert(CurrLine, " inc " + GetByte(AV, 1))
			
		End If
	End If
	
	OutList->CodeEnd = CurrLine
	Return AV
	
End Function

Function CompileConditions (Condition As String, IfTrue As String, Origin As String) As LinkedListElement Pointer
	
	Dim As String V1, V2, Op, R1, R2, Temp, Cmd, BI, VarName, Sum, Reg, CondType
	Dim As Integer PD, Complex, Value1, Value2, OutVal, R2Literal, T, S, WD
	Dim As Integer IsBitTest, IsInverted
	Dim As SubType Pointer CurrSub
	Dim As LinkedListElement Pointer OutList, CurrLine, NewCode
	
	'If Condition = IfTrue, execute next instruction
	
	'Initialise
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Get origin
	'Origin = ""
	IF INSTR(Condition, ";?F") <> 0 THEN 
		Origin = Mid(Condition, INSTR(Condition, ";?F"))
		Condition = Left(Condition, INSTR(Condition, ";?F") - 1)
	END If
	Condition = Trim(Condition)
	CurrSub = Subroutine(GetSubID(Origin))
	'IF Origin = "" THEN PRINT "No Origin!", Condition
	
	'Trim extra brackets on Condition
	If Left(Condition, 1) = "(" And Right(Condition, 1) = ")" Then
		If CountOccur(Condition, "';()") = 2 Then
			Condition = Trim(Mid(Condition, 2, Len(Condition) - 2))
		End If
	End If
	
	'Decide whether to compile inline or using CompileCalc
	'Complex =
	' 0: One condition, byte, compile inline
	' 1: More than one condition or not byte, so compile as calculation
	' -1: No condition, single variable, check if 0 or not 0.
	
	CheckConditionAgain:
	Complex = 0
	CondType = TypeOfValue(Condition, Subroutine(GetSubID(Origin)))
	IF CountOccur(Condition, "';=~<>{}") = 0 THEN Complex = -1
	IF CountOccur(Condition, "';=~<>{}+-*/&|#!") >= 2 THEN Complex = 1
	IF CountOccur(Condition, "';+-*/&|#!") >= 1 THEN Complex = 1
	If CondType <> "BIT" And CondType <> "BYTE" THEN Complex = 1
	'PRINT Condition, GetSub(Origin), IsWord(Condition, GetSub(Origin))
	
	'No conditions - check if variable is 0 or non-0
	'NOT is not taken into account above, need to detect it here
	IF Complex = -1 THEN
		If IsConst(Condition) Then
			If MakeDec(Condition) = 0 Then
				Condition = "AlwaysFalse"
			Else
				Condition = "AlwaysTrue"
			End If
			Return OutList
		Else
			If CondType = "BYTE" Then
				
				IsInverted = 0
				If Left(Condition, 1) = "!" Then
					Condition = Trim(Mid(Condition, 2))
					IsInverted = -1
				End If
				
				If ModePIC Then
					CurrLine = LinkedListInsert(CurrLine, " movf " + Condition + ",F")
					If IsInverted Then
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfsc STATUS,Z"
						IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfss STATUS,Z"
					Else
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfss STATUS,Z"
						IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfsc STATUS,Z"
					End If
					CurrLine = LinkedListInsert(CurrLine, Cmd)
				ElseIf ModeAVR Then
					NewCode = LinkedListCreate
					Reg = PutInRegister(NewCode, Condition, "BYTE", Origin)
					CurrLine = LinkedListInsertList(CurrLine, NewCode)
					CurrLine = LinkedListInsert(CurrLine, " tst " + Reg)
					If IsInverted Then
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " brne PC + 2"
						IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " breq PC + 2"
					Else
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " breq PC + 2"
						IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " brne PC + 2"
					End If
					CurrLine = LinkedListInsert(CurrLine, Cmd)
				End If
				
			Else
				'Checking a single bit
				'Can do this just as efficiently using bit test code in section below
				If InStr(Condition, " ") = 0 Then Condition = Condition + "=1"
				Complex = 0
			End If
		End If
	END If
	
	'One condition and byte, so compile inline
	IF Complex = 0 THEN
		OP = ""
		IsBitTest = CondType = "BIT"
		IF INSTR(Condition, "=") <> 0 THEN OP = "="
		IF INSTR(Condition, "~") <> 0 THEN OP = "~"
		IF INSTR(Condition, "<") <> 0 THEN OP = "<"
		IF INSTR(Condition, ">") <> 0 THEN OP = ">"
		IF INSTR(Condition, "{") <> 0 THEN OP = "{"
		IF INSTR(Condition, "}") <> 0 THEN OP = "}"
		
		'Read input variables
		If OP = "" Then
			V1 = UCase(Left(Condition, INSTR(Condition, " ") - 1))
			V2 = UCase(Mid(Condition, INSTR(Condition, " ") + 1))
			OP = "="
		Else 
			V1 = UCase(Left(Condition, INSTR(Condition, OP) - 1))
			DO WHILE INSTR(V1, " ") <> 0: Replace V1, " ", "": LOOP
			V2 = UCase(Mid(Condition, INSTR(Condition, OP) + 1))
			DO WHILE INSTR(V2, " ") <> 0: Replace V2, " ", "": Loop
		End If
		'Print Condition, ":", V1, OP, V2
		
		'If both V1 and V2 are constants, evaluate condition now
		If IsConst(V1) And IsConst(V2) Then
			Value1 = MakeDec(V1)
			Value2 = MakeDec(V2)
			OutVal = 0
			Select Case OP
			Case "=": OutVal = Value1 = Value2
			Case "~": OutVal = Value1 <> Value2
			Case "<": OutVal = Value1 < Value2
			Case ">": OutVal = Value1 > Value2
			Case "{": OutVal = Value1 <= Value2
			Case "}": OutVal = Value1 >= Value2
			End Select
			
			If OutVal Then
				Condition = "AlwaysTrue"
			Else
				Condition = "AlwaysFalse"
			End If
			
			Return OutList
		End If
		
		'Code for a bit test
		IF IsBitTest THEN
			'If reading from a pin, record it
			Dim As PinDirType Pointer CurrPinDir
			CurrPinDir = GetPinDirection(V1)
			If CurrPinDir <> 0 Then
				CurrPinDir->ReadFrom = -1
			End If
			
			'If V1 is const and V2 is not, swap
			If IsConst(V1) And Not IsConst(V2) Then
				Swap V1, V2
			End If
			
			'Compare bit variable to bit constant
			If IsConst(V2) Then
				
				T = 1
				'Get var and bit
				BI = FixBit(V1, Origin)
				If INSTR(BI, ".") <> 0 Then Replace BI, ".", ","
				'Get state to test
				S = 1: If V2 = "0" THEN S = 0
				If OP = "~" Or OP = ">" Or OP = "<" Then S = 1 - S
				
				If ModePIC Then
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
						Cmd = " btfss "
						IF S = 1 THEN Cmd = " btfsc "
					END IF
					IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
						Cmd = " btfsc "
						IF S = 1 THEN Cmd = " btfss "
					END IF
					
					CurrLine = LinkedListInsert(CurrLine, Cmd + BI)
					If INSTR(BI, ",") <> 0 Then
						VarName = Left(BI, INSTR(BI, ",") - 1)
						AddVar VarName, "BYTE", 1, 0, "REAL", Origin
					End If
				ElseIf ModeAVR Then
					VarName = GetRealIOName(Left(BI, INSTR(BI, ",") - 1))
					BI = Mid(BI, INSTR(BI, ",") + 1)
					If VarName = "SREG" Then
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
							Cmd = " brbs "
							IF S = 1 THEN Cmd = " brbc "
						END IF
						IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
							Cmd = " brbc "
							IF S = 1 THEN Cmd = " brbs "
						END IF
						CurrLine = LinkedListInsert(CurrLine, Cmd + BI + ",PC + 2")
						
					ElseIf IsRegister(VarName) Then
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
							Cmd = " sbrs "
							IF S = 1 THEN Cmd = " sbrc "
						END IF
						IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
							Cmd = " sbrc "
							IF S = 1 THEN Cmd = " sbrs "
						END IF
						CurrLine = LinkedListInsert(CurrLine, Cmd + VarName + "," + BI)
						
					ElseIf IsLowIOReg(VarName) Then
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
							Cmd = " sbis "
							IF S = 1 THEN Cmd = " sbic "
						END IF
						IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
							Cmd = " sbic "
							IF S = 1 THEN Cmd = " sbis "
						END IF
						CurrLine = LinkedListInsert(CurrLine, Cmd + VarName + "," + BI)
						
					ElseIf IsIOReg(VarName) Then
						AddVar "SysCalcTempA", "BYTE", 1, CurrSub, "REAL", Origin
						CurrLine = LinkedListInsert(CurrLine, " in SysCalcTempA," + VarName)
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
							Cmd = " sbrs SysCalcTempA,"
							IF S = 1 THEN Cmd = " sbrc SysCalcTempA,"
						ElseIF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
							Cmd = " sbrc SysCalcTempA,"
							IF S = 1 THEN Cmd = " sbrs SysCalcTempA,"
						END IF
						CurrLine = LinkedListInsert(CurrLine, Cmd + BI)
						
					Else
						AddVar "SysCalcTempA", "BYTE", 1, CurrSub, "REAL", Origin
						CurrLine = LinkedListInsert(CurrLine, " lds SysCalcTempA," + VarName)
						IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
							Cmd = " sbrs SysCalcTempA,"
							IF S = 1 THEN Cmd = " sbrc SysCalcTempA,"
						ElseIF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
							Cmd = " sbrc SysCalcTempA,"
							IF S = 1 THEN Cmd = " sbrs SysCalcTempA,"
						END IF
						CurrLine = LinkedListInsert(CurrLine, Cmd + BI)
						AddVar VarName, "BYTE", 1, CurrSub, "REAL", Origin
						
					End If
				End If
			
			'Need to compare 2 bit variables
			Else
				If ModePIC Then
					'Get variables
					R1 = FixBit(V1, Origin)
					If INSTR(R1, ".") <> 0 Then Replace R1, ".", ","
					R2 = FixBit(V2, Origin)
					If INSTR(R2, ".") <> 0 Then Replace R2, ".", ","
					
					'clear w, xor if each var set
					CurrLine = LinkedListInsert(CurrLine, " clrw")
					CurrLine = LinkedListInsert(CurrLine, " btfsc " + R1)
					CurrLine = LinkedListInsert(CurrLine, " xorlw 255")
					CurrLine = LinkedListInsert(CurrLine, " btfsc " + R2)
					CurrLine = LinkedListInsert(CurrLine, " xorlw 255")
					'If W is 0, variables are equal (inverted 0 or 2 times)
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfss STATUS,Z"
					If INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfsc STATUS,Z"
					CurrLine = LinkedListInsert(CurrLine, Cmd)
										
				ElseIf ModeAVR Then
					'Use recursion, easier!
					'Otherwise, same algorithm as PIC: clr, if, com, if, com, test
					'clear w, xor if each var set
					AddVar "SysCalcTempX", "BYTE", 1, CurrSub, "REAL", Origin
					CurrLine = LinkedListInsert(CurrLine, " clr SysCalcTempX")
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V1 + " = 1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " com SysCalcTempX")
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(V2 + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " com SysCalcTempX")
					'If W is 0, variables are equal (inverted 0 or 2 times)
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " sbrs SysCalcTempX,0"
					If INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " sbrc SysCalcTempX,0"
					CurrLine = LinkedListInsert(CurrLine, Cmd)
					
				End If
				
			End If
			
		'If the condition is not a bit test, then run this code:
		Else
			
			'Add needed variables
			'If using PIC and a bit variable is found, put bit variable into temporary variable
			IF Not IsConst(V1) Then
				If ModePIC Then
					If TypeOfValue(V1, CurrSub) = "BIT" Then
						CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, "SysCalcTempA", Origin))
						V1 = "SysCalcTempA"
					End If
				End If
				AddVar V1, "BYTE", 1, 0, "REAL", Origin
			End If
			IF NOT IsConst(V2) Then
				If ModePIC Then
					If TypeOfValue(V2, CurrSub) = "BIT" Then
						CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V2, "SysCalcTempB", Origin))
						V2 = "SysCalcTempB"
					End If
				End If
				AddVar V2, "BYTE", 1, 0, "REAL", Origin
			End If
			
			'Remove any casts
			If InStr(V1, "]") <> 0 Then
				V1 = Trim(Mid(V1, InStr(V1, "]") + 1))
			End If
			If InStr(V2, "]") <> 0 Then
				V2 = Trim(Mid(V2, InStr(V2, "]") + 1))
			End If
			
			'Code to run comparison
			'Used by all AVR compare modes
			If ModeAVR Then
				'Swap V1/V2 as needed
				IF ((OP = "=" Or OP = "~") And IsConst(V1)) OR OP = ">" OR OP = "{" Then
					Swap V1, V2
				End If
				
				'Put V1 in SysCalcTempA
				If IsRegister(V1) Then
					R1 = V1
				Else
					R1 = "SysCalcTempA"
					AddVar R1, "BYTE", 1, CurrSub, "REAL", Origin
					CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(V1, "SysCalcTempA", Origin))
				End If
				'ElseIf IsIOReg(V1) Then
				'	R1 = "SysCalcTempA"
				'	CurrLine = LinkedListInsert(CurrLine, " in SysCalcTempA," + V1)
				'ElseIf IsConst(V1) Then
				'	R1 = "SysCalcTempA"
				'	CurrLine = LinkedListInsert(CurrLine, " ldi SysCalcTempA," + Str(MakeDec(V1)))
				'Else
				'	R1 = "SysCalcTempA"
				'	CurrLine = LinkedListInsert(CurrLine, " lds SysCalcTempA," + V1)
				'	AddVar V1, "BYTE", 1, 0, "REAL", Origin
				'End If
				
				'Put V2 in SysCalcTempB
				R2Literal = 0
				If IsRegister(V2) Then
					R2 = V2
				ElseIf IsIOReg(V2) Then
					R2 = "SysCalcTempB"
					AddVar R2, "BYTE", 1, CurrSub, "REAL", Origin
					CurrLine = LinkedListInsert(CurrLine, " in SysCalcTempB," + V2)
				ElseIf IsConst(V2) Then
					R2 = Str(MakeDec(V2))
					R2Literal = -1
				Else
					R2 = "SysCalcTempB"
					AddVar R2, "BYTE", 1, CurrSub, "REAL", Origin
					CurrLine = LinkedListInsert(CurrLine, " lds SysCalcTempB," + V2)
					AddVar V2, "BYTE", 1, CurrSub, "REAL", Origin
				End If
				
				'Compare
				If R2Literal Then
					CurrLine = LinkedListInsert(CurrLine, " cpi " + R1 + "," + R2)
				Else
					CurrLine = LinkedListInsert(CurrLine, " cp " + R1 + "," + R2)
				End If
					
			End If
			
			'Code for equal/not equal
			IF OP = "=" OR OP = "~" THEN
				
				If ModePIC Then
					'V1 must always be a variable. V2 can be variable or constant.
					IF IsConst(V1) Then
						Temp = V1
						V1 = V2
						V2 = Temp
					End If
					
					If IsConst(V2) And MakeDec(V2) = 0 Then
						CurrLine = LinkedListInsert(CurrLine, " movf " + V1 + ",F")
					ElseIf IsConst(V2) And MakeDec(V2) = 1 Then
						CurrLine = LinkedListInsert(CurrLine, " decf " + V1 + ",W")
					ElseIf IsConst(V2) And MakeDec(V2) = 255 Then
						CurrLine = LinkedListInsert(CurrLine, " incf " + V1 + ",W")
					Else
						IF IsConst(V2) Then
							CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V2)))
						Else
							CurrLine = LinkedListInsert(CurrLine, " movf " + V2 + ",W")
						End If
						CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
					End If
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
						Cmd = " btfss STATUS, Z"
						IF OP = "=" THEN Cmd = " btfsc STATUS, Z"
					END IF
					IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
						Cmd = " btfsc STATUS, Z"
						IF OP = "=" THEN Cmd = " btfss STATUS, Z"
					END IF
					CurrLine = LinkedListInsert(CurrLine, Cmd)
				ElseIf ModeAVR Then
					
					'Compare is inserted above
					
					'Branch
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN
						Cmd = " breq PC + 2"
						IF OP = "=" THEN Cmd = " brne PC + 2"
					END IF
					IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN
						Cmd = " brne PC + 2"
						IF OP = "=" THEN Cmd = " breq PC + 2"
					END IF
					CurrLine = LinkedListInsert(CurrLine, Cmd)
					
				END IF
			END IF
			
			'Code for greater than/less than
			IF OP = "<" OR OP = ">" THEN
				'COSC = COSC + 1: CheckTemp(COSC) = " bsf STATUS, C"
				If ModePIC Then
					IF OP = ">" THEN Swap V1, V2
					'Take bigger from smaller. V2 should be bigger. Take V2 from V1
					'mov small into W
					'sub big from W
					IF IsConst(V1) And ChipFamily = 12 THEN
						If InStr(V1, "@") <> 0 Then
							CurrLine = LinkedListInsert(CurrLine, " movlw " + V1)
						Else
							CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V1)))
						End If
						CurrLine = LinkedListInsert(CurrLine, " movwf SysIFTemp")
						V1 = "SysIFTemp": AddVar V1, "BYTE", 1, 0, "REAL", Origin
					END If
					
					IF IsConst(V2) THEN Cmd = " movlw " + Str(MakeDec(V2))
					IF NOT IsConst(V2) THEN Cmd = " movf " + V2 + ",W"
					CurrLine = LinkedListInsert(CurrLine, Cmd)
					
					If IsConst(V1) Then
						CurrLine = LinkedListInsert(CurrLine, " sublw " + Str(MakeDec(V1)))
					Else
						CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
					End If
					
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfss STATUS, C"
					IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfsc STATUS, C"
					CurrLine = LinkedListInsert(CurrLine, Cmd)
				
				ElseIf ModeAVR Then
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brsh PC + 2")
					IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brlo PC + 2")
					
				End If
			END IF
			
			'Code for less or equal/more or equal
			IF OP = "{" OR OP = "}" THEN
				If ModePIC Then
					IF OP = "{" THEN Swap V1, V2
					'Take smaller from bigger. V1 should be bigger or equal. Take V2 from V1
					'mov small into W
					'sub big from W
					
					IF IsConst(V1) And ChipFamily = 12 THEN
						If InStr(V1, "@") <> 0 Then
							CurrLine = LinkedListInsert(CurrLine, " movlw " + V1)
						Else
							CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(MakeDec(V1)))
						End If
						CurrLine = LinkedListInsert(CurrLine, " movwf SysIFTemp")
						V1 = "SysIFTemp": AddVar V1, "BYTE", 1, 0, "REAL", Origin
					END If
					
					IF IsConst(V2) Then
						If InStr(V2, "@") <> 0 Then
							Cmd = " movlw " + V2
						Else
							Cmd = " movlw " + Str(MakeDec(V2))
						End If
					End If
					IF NOT IsConst(V2) THEN Cmd = " movf " + V2 + ",W"
					CurrLine = LinkedListInsert(CurrLine, Cmd)
					
					'CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
					If IsConst(V1) Then
						CurrLine = LinkedListInsert(CurrLine, " sublw " + Str(MakeDec(V1)))
					Else
						CurrLine = LinkedListInsert(CurrLine, " subwf " + V1 + ",W")
					End If
					
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfsc STATUS, C"
					IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfss STATUS, C"
					CurrLine = LinkedListInsert(CurrLine, Cmd)
					
				ElseIf ModeAVR Then
					IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brlo PC + 2")
					IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN CurrLine = LinkedListInsert(CurrLine, " brsh PC + 2")
				End If
			END IF
		
		END IF
		
	End IF
	
	'More than one condition or not byte, so compile as calculation
ComplexCondition:
	IF Complex = 1 THEN
		DO WHILE INSTR(Condition, " ") <> 0: Replace Condition, " ", "": LOOP
		T = 0
		Sum = Condition
		FreeCalcVar ""
		
		Dim As CodeSection Pointer CodeDest
		CodeDest = 0
		CompileCalc(Sum, "", Origin, CodeDest)
		NewCode = CodeDest->CodeList
		If NewCode->Next <> 0 Then
			CurrLine = LinkedListInsertList(CurrLine, NewCode)
			If ModePIC Then
				IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " btfsc " + Sum + ",0"
				IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " btfss " + Sum + ",0"
			ElseIf ModeAVR Then
				IF INSTR(UCase(IfTrue), "TRUE") <> 0 THEN Cmd = " sbrc " + Sum + ",0"
				IF INSTR(UCase(IfTrue), "FALSE") <> 0 THEN Cmd = " sbrs " + Sum + ",0"
			End If
			CurrLine = LinkedListInsert(CurrLine, Cmd)
		Else
			If MakeDec(Sum) = 0 Then
				Condition = "AlwaysFalse"
			Else
				Condition = "AlwaysTrue"
			End If
			Return OutList
		End If
	END IF
	
	Return OutList
End Function

Sub CompileDim (CurrSub As SubType Pointer)
	Dim As String VarName, VarType, VarAlias, VarFixedLocIn, Origin, InLine
	Dim As Integer PD, IsAlias, Si, CD, VarFixedLoc, NewVarCount
	Dim As LinkedListElement Pointer CurrLine
	
	'Don't try to read dims from macro
	'Only read from macro invocation
	If CurrSub->IsMacro Then Return
	
	Dim NewVarList(100) As String
	NewVarCount = 0
	
	'Mark sub as having had variables read
	'Haven't actually read them yet (won't until end of this sub)
	'But might cause an infinite loop if this isn't set
	CurrSub->VarsRead = -1
	
	CurrLine = CurrSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = CurrLine->Value
		
		If Left(InLine, 4) = "DIM " THEN
			InLine = Mid(InLine, 5)
			CurrLine = LinkedListDelete(CurrLine)
			
			'Get origin
			Origin = ""
			If INSTR(InLine, ";?F") <> 0 THEN 
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			'Get name
			VarName = InLine
			If INSTR(VarName, " AS ") <> 0 THEN VarName = Left(VarName, INSTR(VarName, " AS ") - 1)
			If INSTR(VarName, " ALIAS ") <> 0 THEN VarName = Left(VarName, INSTR(VarName, " ALIAS ") - 1)
			If INSTR(VarName, " AT ") <> 0 THEN VarName = Left(VarName, INSTR(VarName, " AT ") - 1)
			VarName = Trim(VarName)
			If INSTR(VarName, ",") = 0 Then
				NewVarList(1) = VarName
				NewVarCount = 1
			Else
				NewVarCount = 0
				Do While INSTR(VarName, ",") <> 0
					NewVarCount += 1
					NewVarList(NewVarCount) = Trim(Left(VarName, INSTR(VarName, ",") - 1))
					VarName = Trim(Mid(VarName, INSTR(VarName, ",") + 1))
				Loop
				NewVarCount += 1
				NewVarList(NewVarCount) = VarName
			End If
			
			'Get type
			VarType = "BYTE"
			IF INSTR(InLine, " AS ") <> 0 Then
				VarType = Mid(InLine, INSTR(InLine, " AS ") + 3)
				IF INSTR(VarType, "(") <> 0 Then VarType = Left(VarType, INSTR(VarType, "(") - 1)
				IF INSTR(VarType, " ALIAS ") <> 0 Then VarType = Left(VarType, INSTR(VarType, " ALIAS ") - 1)
				IF INSTR(VarType, " AT ") <> 0 Then VarType = Left(VarType, INSTR(VarType, " AT ") - 1)
				VarType = Trim(UCase(VarType))
			END IF
			
			'Check if alias
			IsAlias = 0
			IF INSTR(InLine, " ALIAS ") <> 0 Then
				IsAlias = -1
				VarAlias = Trim(Mid(InLine, INSTR(InLine, " ALIAS ") + 7))
				IF INSTR(VarAlias, " AS ") <> 0 Then VarAlias = Left(VarAlias, INSTR(VarAlias, " AS ") - 1)
				IF INSTR(VarAlias, " AT ") <> 0 Then VarAlias = Left(VarAlias, INSTR(VarAlias, " AT ") - 1)
			End If
			
			'Check if fixed location specified
			VarFixedLoc = -1
			If InStr(InLine, " AT ") <> 0 Then
				VarFixedLocIn = Trim(Mid(InLine, INSTR(InLine, " AT ") + 4))
				IF INSTR(VarFixedLocIn, " AS ") <> 0 Then VarFixedLocIn = Left(VarFixedLocIn, INSTR(VarFixedLocIn, " AS ") - 1)
				IF INSTR(VarFixedLocIn, " ALIAS ") <> 0 Then VarFixedLocIn = Left(VarFixedLocIn, INSTR(VarFixedLocIn, " ALIAS ") - 1)
				
				'Is a calculation needed to get location?
				If IsCalc(VarFixedLocIn) Then
					Calculate VarFixedLocIn
				End If
				
				'Is fixed location given as a constant?
				If IsConst(VarFixedLocIn) Then
					VarFixedLoc = MakeDec(VarFixedLocIn)
				'Is fixed location given as a reference to an SFR?
				Else
					'Not supported, but can be added later if needed
				End If
				'Print "Var loc:"; VarFixedLoc
			End If
			
			For CV = 1 to NewVarCount
				InLine = NewVarList(CV)
				'Get size
				Si = 1
				IF INSTR(InLine, "(") <> 0 Then
					Si = VAL(Mid(InLine, INSTR(InLine, "(") + 1))
					InLine = Trim(Left(InLine, INSTR(InLine, "(") - 1))
				End If
				
				'Add var
				If IsAlias Then
					AddVar(InLine, VarType, Si, CurrSub, "ALIAS:" + VarAlias, Origin, , -1)
				Else
					AddVar(InLine, VarType, Si, CurrSub, "REAL", Origin, VarFixedLoc, -1)
				End If
			Next
		End If
		
		CurrLine = CurrLine->Next
	Loop
	
End Sub

SUB CompileDir (CompSub As SubType Pointer)
	
	Dim As String Origin, InLine, VarName, Port, Temp, Direction, TempOld
	Dim As String TrisPort
	Dim As Integer DirPort, NotIOPort, SearchPos, FoundPos
	Dim As LinkedListElement Pointer CurrLine
	Dim As PinDirType Pointer CurrPinDir
	
	FoundCount = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = CurrLine->Value
		IF Left(InLine, 4) = "DIR " THEN
			
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			FoundCount += 1
			InLine = Trim(Mid(InLine, 4))
			Direction = LTrim(Mid(InLine, INSTR(InLine, " ") + 1))
			Port = RTrim(Left(InLine, INSTR(InLine, " ") - 1))
			
			'Handle original (early 2006) form of Dir command (ie. Dir B0 Output)
			IF INSTR(Port, "PORT") = 0 AND (Left(ChipName, 2) = "16" Or Left(ChipName, 2) = "18") AND LEN(Port) = 2 THEN
				Port = "PORT" + Left(Port, 1) + "." + Mid(Port, 2)
			'early 2006 version of full port (ie. Dir B number)
			ElseIf Len(Port) = 1 AND (Left(ChipName, 2) = "16" Or Left(ChipName, 2) = "18") Then
				Port = "PORT" + Port
			End If
			If Left(ChipName, 2) = "10" Or Left(ChipName, 2) = "12" Then
				If Port = "" Or Port = "IO" Then
					Port = "GPIO"
				End If
			End If
			Replace Direction, "PUT", ""
			
			'Get the variable that sets port direction
			VarName = Port
			Replace VarName, "PORT", ""
			Replace VarName, "GP", ""
			TrisPort = VarName
			If InStr(TrisPort, ".") <> 0 Then TrisPort = Left(TrisPort, InStr(TrisPort, ".") - 1)
			If ModePIC Then
				VarName = "TRIS" + VarName
			ElseIf ModeAVR Then
				VarName = "DDR" + VarName
			'ElseIf ModeZ8 Then
			'	VarName = Mid(VarName, 2, 1) 'Get letter after p 
			End If
			
			'Determine if DIR sets 1 bit or whole port
			DirPort = -1
			IF INSTR(Port, ".") <> 0 THEN DirPort = 0
			
			'Check that a pin is being set
			Temp = VarName
			Replace Temp, "TRIS", ""
			Replace Temp, "DDR", ""
			TempOld = Temp
			IF INSTR(Temp, ".") <> 0 THEN Temp = Left(Temp, INSTR(Temp, ".") - 1)
			NotIOPort = 0
			IF LEN(Temp) <> 1 AND Temp <> "IO" THEN NotIOPort = -1
			
			Temp = VarName
			If Instr(Temp, ".") <> 0 Then Temp = Left(Temp, Instr(Temp, ".") - 1)
			'If ChipFamily <> 12 And Not IsIOReg(Temp) Then NotIOPort = -1
			
			If NotIOPort Then
				Temp = Message("NotIO")
				Replace Temp, "%var%", Port
				LogError Temp, Origin
			End If
			
			'Add code
			'Whole port mode
			IF DirPort THEN
				
				'Record settings
				CurrPinDir = GetPinDirection(Port)
				If CurrPinDir <> 0 Then
					If Direction = "IN" Then
						CurrPinDir->SetIn = -1
						CurrPinDir->WholePort = -1
						CurrPinDir->WholePortDir = 255
					ElseIf Direction = "OUT" Then
						CurrPinDir->SetOut = -1
						CurrPinDir->WholePort = -1
						CurrPinDir->WholePortDir = 0
					Else
						'If port is set to mixed direction, store direction
						If IsConst(Direction) Then
							CurrPinDir->WholePort = -1
							CurrPinDir->WholePortDir = MakeDec(Direction)
						Else
							CurrPinDir->WholePort = -1
							CurrPinDir->WholePortDir = -1
						End If
					End If
				End If
				
				'Generate code
				IF Direction = "IN" THEN
					If ModePIC Then
						If ChipFamily = 12 And Not IsIOReg(VarName) Then
							CurrLine->Value = " movlw 255"
							CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
						Else
							If PSP = 1 AND VarName = "TRISE" Then
								CurrLine->Value = VarName + "=239"
							Else
								CurrLine->Value = VarName + "=255"
							End If
						End If
					ElseIf ModeAVR Then
						CurrLine->Value = " ldi SysValueCopy,0"
						CurrLine = LinkedListInsert(CurrLine, " out " + VarName + ",SysValueCopy")
					End If
					
				ElseIf Direction = "OUT" THEN
					If ModePIC Then
						If ChipFamily = 12 And Not IsIOReg(VarName) Then
							CurrLine->Value = " clrw"
							CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
						Else
							CurrLine->Value = " clrf " + VarName
						End If
					ElseIf ModeAVR Then
						CurrLine->Value = " ldi SysValueCopy,255"
						CurrLine = LinkedListInsert(CurrLine, " out " + VarName + ",SysValueCopy")
					End If
					
				Else
					If ChipFamily = 12 And Not IsIOReg(VarName) Then
						If IsConst(Direction) Then
							CurrLine->Value = " movlw " + Direction
							CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
						Else
							CurrLine->Value = " movf " + Direction + ",W"
							CurrLine = LinkedListInsert(CurrLine, " tris " + Port)
						End If	
					Else
						CurrLine->Value = VarName + "=" + Direction
					End If
				End If
			
			'Single pin mode
			Else
				'On 12 bit PIC, need to create shadow register
				'Can't set individual pin directions otherwise
				If ChipFamily = 12 Then
					FoundPos = -1
					For SearchPos = 1 To PinDirShadows
						If PinDirShadow(SearchPos) = TrisPort Then
							FoundPos = SearchPos
							Exit For
						End If
					Next
					If FoundPos = -1 Then
						PinDirShadows += 1
						PinDirShadow(PinDirShadows) = TrisPort
					End If
				End If
				
				'Record setting
				CurrPinDir = GetPinDirection(Port)
				If CurrPinDir <> 0 Then
					If Direction = "IN" Then
						CurrPinDir->SetIn = -1
						
						'Check that pin can be input
						If CurrPinDir->AllowedDirections <> "" And InStr(CurrPinDir->AllowedDirections, "i") = 0 Then
							Temp = Message("WarningPinNotInput")
							Replace Temp, "%pin%", Port
							LogWarning Temp, Origin
						End If
						
					ElseIf Direction = "OUT" Then
						CurrPinDir->SetOut = -1
						
						'Check that pin can be output
						If CurrPinDir->AllowedDirections <> "" And InStr(CurrPinDir->AllowedDirections, "o") = 0 Then
							Temp = Message("WarningPinNotOutput")
							Replace Temp, "%pin%", Port
							LogWarning Temp, Origin
						End If
						
					End If
				End If
				
				'Generate code
				Replace VarName, ".", ","
				If ModePIC Then
					If Direction = "IN" Then
						CurrLine->Value = " bsf " + VarName
					ElseIf Direction = "OUT" Then
						CurrLine->Value = " bcf " + VarName
					Else
						LogError Message("BadDirection"), Origin
					End If
				ElseIf ModeAVR Then
					If Direction = "IN" Then
						CurrLine->Value = " cbi " + VarName
					ElseIf Direction = "OUT" Then
						CurrLine->Value = " sbi " + VarName
					Else
						LogError Message("BadDirection"), Origin
					End If
				End If
			End If
			
		End If
		CurrLine = CurrLine->Next
	Loop
END SUB

SUB CompileDo (CompSub As SubType Pointer)
	FoundCount = 0
	Dim As String InLine, Temp, Origin, Mode, Condition, LoopOrigin
	Dim As Integer DL, CP, ExitFound, WD
	Dim As LinkedListElement Pointer CurrLine, FindLoop, LoopLoc, NewCode
	
	DL = 0
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = UCase(CurrLine->Value)
		If Left(InLine, 3) = "DO " OR InLine = "DO" THEN
			
			'Get origin
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN 
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			'Get position of condition
			CP = 0
			IF Left(InLine, 3) = "DO " THEN CP = 1
			DL = 1 'Nesting level
			ExitFound = 0 'Exit Do found?
			DLC = DLC + 1 'Increment do loop count
			CurrLine->Value = "SysDoLoop_S" + Str(DLC) + LabelEnd 'Add label to mark start
			
			'Find exit do, loop
			LoopLoc = 0
			FindLoop = CurrLine->Next
			Do While FindLoop <> 0
				Temp = UCase(FindLoop->Value)
				IF Left(Temp, 5) = "LOOP " OR Temp = "LOOP" THEN DL = DL - 1
				IF Left(Temp, 3) = "DO " OR Temp = "DO" THEN DL = DL + 1
				IF Left(Temp, 7) = "EXIT DO" AND DL = 1 Then
					ExitFound = 1
					If ModePIC Then FindLoop->Value = " goto SysDoLoop_E" + Str(DLC)
					If ModeAVR Then FindLoop->Value = " rjmp SysDoLoop_E" + Str(DLC)
					If ModeZ8 Then FindLoop->Value = " jp SysDoLoop_E" + Str(DLC)
				End If
				IF DL = 0 THEN LoopLoc = FindLoop: Exit Do
				FindLoop = FindLoop->Next
			Loop
			
			'Show error if no loop
			If LoopLoc = 0 Then
				
			'Otherwise, compile normally
			Else
				'Get origin of LOOP command
				LoopOrigin = ""
				IF INSTR(LoopLoc->Value, ";?F") <> 0 THEN 
					LoopOrigin = Mid(InLine, INSTR(InLine, ";?F"))
					LoopLoc->Value = RTrim(Left(LoopLoc->Value, INSTR(LoopLoc->Value, ";?F") - 1))
				END If
				If LoopOrigin = "" Then LoopOrigin = Origin
				
				'Is the condition after the LOOP?
				IF Left(LoopLoc->Value, 5) = "LOOP " THEN CP = 2
				
				'Get mode {UNTIL | WHILE}, condition
				IF CP = 1 THEN
					Mode = LTrim(Mid(InLine, 4))
					Condition = LCase(Mid(Mode, INSTR(Mode, " ") + 1))
					Mode = UCase(Left(Mode, INSTR(Mode, " ") - 1))
				END IF
				IF CP = 2 THEN
					Mode = LTrim(Mid(LoopLoc->Value, 6))
					Condition = LCase(Mid(Mode, INSTR(Mode, " ") + 1))
					Mode = UCase(Left(Mode, INSTR(Mode, " ") - 1))
				END IF
				
				'Delete Loop
				LoopLoc = LinkedListDelete(LoopLoc)
								
				'Compile with no condition
				If CP = 0 Then
					If ModePIC Then Temp = " goto SysDoLoop_S" + Str(DLC)
					If ModeAVR Then Temp = " rjmp SysDoLoop_S" + Str(DLC)
					If ModeZ8 Then Temp = " jp SysDoLoop_S" + Str(DLC)
					LoopLoc = LinkedListInsert(LoopLoc, Temp)
						
				'Compile with condition after DO
				ElseIf CP = 1 THEN
					Temp = "TRUE": IF Mode = "WHILE" THEN Temp = "FALSE"
					NewCode = CompileConditions(Condition, Temp, Origin)
					If (Mode = "WHILE" And Condition <> "AlwaysTrue") Or (Mode = "UNTIL" And Condition <> "AlwaysFalse") Then
						CurrLine = LinkedListInsertList(CurrLine, NewCode)
						If ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto SysDoLoop_E" + Str(DLC))
						If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp SysDoLoop_E" + Str(DLC))
						If ModeZ8 Then CurrLine = LinkedListInsert(CurrLine, " jp SysDoLoop_E" + Str(DLC))
					End If
					
					If ModePIC Then Temp = " goto SysDoLoop_S" + Str(DLC)
					If ModeAVR Then Temp = " rjmp SysDoLoop_S" + Str(DLC)
					If ModeZ8 Then Temp = " jp SysDoLoop_S" + Str(DLC)
					LoopLoc = LinkedListInsert(LoopLoc, Temp)
								
				'Compile with condition after LOOP
				ElseIf CP = 2 THEN
					Temp = "TRUE": IF Mode = "UNTIL" THEN Temp = "FALSE"
					NewCode = CompileConditions(Condition, Temp, LoopOrigin)
					If (Mode = "WHILE" And Condition <> "AlwaysFalse") Or (Mode = "UNTIL" And Condition <> "AlwaysTrue") Then
						'FOR WD = 1 TO COSC
						'	AddLine CheckTemp(WD), LL + WD - 1
						'NEXT WD
						'LL = LL + COSC
						LoopLoc = LinkedListInsertList(LoopLoc, NewCode)
						
						If ModePIC Then Temp = " goto SysDoLoop_S" + Str(DLC)
						If ModeAVR Then Temp = " rjmp SysDoLoop_S" + Str(DLC)
						If ModeZ8 Then Temp = " jp SysDoLoop_S" + Str(DLC)
						LoopLoc = LinkedListInsert(LoopLoc, Temp)
					END IF
				End If
				
				'Create label for end of loop
				LoopLoc = LinkedListInsert(LoopLoc, "SysDoLoop_E" + Str(DLC) + LabelEnd)
			End IF
		End If
		
		CurrLine = CurrLine->Next
	Loop
	FoundCount = DLC
	
END SUB

SUB CompileExitSub (CompSub As SubType Pointer)
   
	Dim As String Temp
	Dim As LinkedListElement Pointer CurrLine
	FoundCount = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		IF Left(UCase(CurrLine->Value), 8) = "EXIT SUB" OR Left(UCase(CurrLine->Value), 13) = "EXIT FUNCTION" THEN
			If ModePIC Then CurrLine->Value = " return"
			If ModeAVR Then CurrLine->Value = " ret"
			If ModeZ8 Then CurrLine->Value = " ret"
			FoundCount = FoundCount + 1
		End If
		
		IF Left(UCase(CurrLine->Value), 4) = "END " THEN 
			Temp = CurrLine->Value
			IF INSTR(Temp, ";") <> 0 THEN Temp = Trim(Left(Temp, INSTR(Temp, ";") - 1))
			IF Temp = "END" THEN
				If ModePIC Then CurrLine->Value = " goto BASPROGRAMEND"
				If ModeAVR Then CurrLine->Value = " rjmp BASPROGRAMEND"
				If ModeZ8 Then CurrLine->Value = " jp BASPROGRAMEND"
				FoundCount = FoundCount + 1
			END IF
		END IF
		CurrLine = CurrLine->Next
	Loop
   
END SUB

SUB CompileFor (CompSub As SubType Pointer)
	
	Dim As String InLine, Origin, Temp, ErrorOrigin
	Dim As String LoopVar, StartValue, EndValue, StepValue, LoopVarType
	Dim As Integer FL, CD
	Dim As LinkedListElement Pointer CurrLine, FindLoop, LoopLoc
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		InLine = CurrLine->Value
		
		IF Left(InLine, 4) = "FOR " THEN
			FL = 0
			
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			Replace InLine, " TO ", Chr(30)
			IF INSTR(InLine, " STEP ") <> 0 THEN Replace InLine, " STEP ", Chr(31)
			DO WHILE INSTR(InLine, " ") <> 0: Replace InLine, " ", "": LOOP
			LoopVar = Mid(InLine, 4)
			LoopVar = Left(LoopVar, INSTR(LoopVar, "=") - 1)
			StartValue = Mid(InLine, INSTR(InLine, "=") + 1)
			StartValue = Left(StartValue, INSTR(StartValue, Chr(30)) - 1)
			EndValue = Mid(InLine, INSTR(InLine, Chr(30)) + 1)
			StepValue = "1"
			IF INSTR(InLine, Chr(31)) <> 0 THEN
				EndValue = Left(EndValue, INSTR(EndValue, Chr(31)) - 1)
				StepValue = Mid(InLine, INSTR(InLine, Chr(31)) + 1)
			END IF
			'Print "For variable:"; LoopVar; " start:"; StartValue; " end:"; EndValue; " step:"; StepValue
			
			'Check that variable can hold start and end values
			LoopVarType = TypeOfValue(LoopVar, CompSub)
			If CastOrder(LoopVarType) < CastOrder(TypeOfValue(StartValue, CompSub)) Then
				LogError(Message("ForBadStart"), Origin)
			End If
			If CastOrder(LoopVarType) < CastOrder(TypeOfValue(EndValue, CompSub)) Then
				LogError(Message("ForBadEnd"), Origin)
			End If
			
			'Negate step value if necessary
			IF VAL(EndValue) < VAL(StartValue) AND IsConst(EndValue) AND IsConst(StartValue) AND INSTR(StepValue, "-") = 0 THEN StepValue = "-" + StepValue
			
			FLC = FLC + 1
			FL = 1
			
			'old Pseudo code:
			'V = SV - ST
			'SysForLoop(n):
			'V += ST
			'...
			'if V < EV then goto SysForLoop(n)
			
			'Changed to this: (27/6/2010)
			'V = SV
			'SysForLoop(n):
			'if V > EV then goto SysForLoopEnd(n)
			'...
			'V += ST
			'goto SysForLoop(n)
			'(New version more in line with For in other dialects)
			
			'Then to this: (27/8/2010)
			'V = SV - ST
			'If SV not const or EV not const, if V > EV then goto SysForLoopEnd(n)
			'SysForLoop(n):
			'V += ST
			'...
			'if V <= EV then goto SysForLoop(n)
			'SysForLoopEnd(n):
			
			'Starting code
			CurrLine->Value = LoopVar + "=" + StartValue + "-" + StepValue + Origin + "[ao]"
			
			If Not IsConst(EndValue) Or Not IsConst(StartValue) Then
				If INSTR(StepValue, "-") = 0 THEN
					CurrLine = LinkedListInsert(CurrLine, "IF " + StartValue + " > " + EndValue + " THEN" + Origin)
				Else
					CurrLine = LinkedListInsert(CurrLine, "IF " + StartValue + " < " + EndValue + " THEN" + Origin)
				End If
				IF ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto SysForLoopEnd" + Str(FLC))
				IF ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp SysForLoopEnd" + Str(FLC))
				CurrLine = LinkedListInsert(CurrLine, "END IF")
			End If
			CurrLine = LinkedListInsert(CurrLine, "SysForLoop" + Str(FLC) + LabelEnd)
			CurrLine = LinkedListInsert(CurrLine, LoopVar + "=" + LoopVar + "+" + StepValue + Origin)
			
			'End code
			'Find matching Next
			LoopLoc = 0
			FindLoop = CurrLine->Next
			Do While FindLoop <> 0
				InLine = UCase(FindLoop->Value)
				
				'Nested For, increment level counter, ensure different counter var used
				IF Left(InLine, 4) = "FOR " THEN
					FL += 1
					Temp = Mid(InLine, 5)
					Temp = Trim(Left(Temp, INSTR(Temp, "=") - 1))
					IF Temp = UCase(LoopVar) Then
						ErrorOrigin = ""
						IF INSTR(InLine, ";?F") <> 0 THEN ErrorOrigin = Mid(InLine, INSTR(InLine, ";?F"))
						Temp = Message("ForIndexInUse")
						Replace Temp, "%index%", LoopVar
						LogError Temp, ErrorOrigin
					END IF
				
				ElseIf Left(InLine, 8) = "EXIT FOR" AND FL = 1 Then
					IF ModePIC Then FindLoop->Value = " goto SysForLoopEnd" + Str(FLC)
					IF ModeAVR Then FindLoop->Value = " rjmp SysForLoopEnd" + Str(FLC)
				
				ElseIF InLine = "NEXT" Or Left(InLine, 5) = "NEXT " THEN
					FL -= 1
				END IF
				
				IF FL = 0 THEN LoopLoc = FindLoop: EXIT Do
				FindLoop = FindLoop->Next
			Loop
			
			IF LoopLoc = 0 THEN
				LogError Message("NoNext"), Origin
			
			Else
				LoopLoc = LinkedListDelete(LoopLoc)
				If INSTR(StepValue, "-") = 0 THEN
					LoopLoc = LinkedListInsert(LoopLoc, "IF " + LoopVar + " < " + EndValue + " THEN" + Origin)
				Else
					LoopLoc = LinkedListInsert(LoopLoc, "IF " + LoopVar + " > " + EndValue + " THEN" + Origin)
				End If
				IF ModePIC Then
					LoopLoc = LinkedListInsert(LoopLoc, " goto SysForLoop" + Str(FLC))
				ElseIf ModeAVR Then
					LoopLoc = LinkedListInsert(LoopLoc, " rjmp SysForLoop" + Str(FLC))
				End If
				LoopLoc = LinkedListInsert(LoopLoc, "END IF" + Str(FLC) + LabelEnd)
				
				LoopLoc = LinkedListInsert(LoopLoc, "SysForLoopEnd" + Str(FLC) + LabelEnd)
			End IF
		END If
		
		CurrLine = CurrLine->Next
	Loop
	
	FoundCount = FLC
End SUB

Sub CompileGoto (CompSub As SubType Pointer)
	'Compile GOSUBs, RETURNs, GOTOs and labels
	Dim As String InLine, DestLabel, Origin
	'Dim As Integer PD
	Dim As LinkedListElement Pointer CurrLine
	
	FoundCount = 0
	
	'Find labels, alter on PICs
	If ModePIC Then
		CurrLine = CompSub->CodeStart->Next
		Do While CurrLine <> 0
			IF Right(CurrLine->Value, 1) = ":" THEN
				Replace CurrLine->Value, ":", ""
				FoundCount += 1
			End If
			CurrLine = CurrLine->Next
		Loop
	End If
	
	'Find and translate everything else
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = UCASE(CurrLine->Value)
		
		If Left(InLine, 5) = "GOTO " THEN
			IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
			Else
				Origin = ""
			End If
			DestLabel = Trim(MID(CurrLine->Value, 6))
			
			'Should check label!
			If Right(DestLabel, 1) = ":" Then DestLabel = Trim(Left(DestLabel, Len(DestLabel) - 1))
			
			If ModePIC Then
				CurrLine->Value = " goto " + DestLabel
			ElseIf ModeAVR Then
				CurrLine->Value = " rjmp " + DestLabel
			ElseIf ModeZ8 Then
				CurrLine->Value = " jp " + DestLabel
			End If
			FoundCount += 1
		
		ElseIf Left(InLine, 6) = "GOSUB " THEN
			IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
			Else
				Origin = ""
			End If
			DestLabel = Trim(MID(CurrLine->Value, 7))
			
			CurrLine->Value = " call " + DestLabel
			FoundCount += 1
			
		ElseIf Left(InLine, 6) = "RETURN" THEN
			'Remove origin
			IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
			Else
				Origin = ""
			End If
			
			''If in a function, check for value after return
			'If CompSub->IsFunction Then
			'	Dim Value As String
			'	Value = Trim(Mid(CurrLine->Value, 7))
			'	If Value <> "" Then
			'		
			'	End If
			'End If
			
			If ModePIC Then
				CurrLine->Value = " return"
			ElseIf ModeAVR Or ModeZ8 Then
				CurrLine->Value = " ret"
			End If
			FoundCount += 1
		END IF
		CurrLine = CurrLine->Next
	Loop
	
End Sub

SUB CompileIF (CompSub As SubType Pointer)
	FoundCount = 0
	Dim As String Origin, Temp, L1, L2, Condition, EndBlock
	Dim As Integer FoundIF, IL, ELC, IT, TL, FE, DelSection, DelEndIf
	Dim As Integer WD
	
	Dim As LinkedListElement Pointer CurrLine, NewCode, FindEnd, StartDel
	
	'Needs to loop through program several times, compile nested IFs one level at a time
	COMPILEIFS:
	
	FoundIF = 0
	IL = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		'IF
		IF UCase(Left(CurrLine->Value, 3)) = "IF " THEN
		 
			IL = IL + 1
			IF IL = 1 THEN
				ILC = ILC + 1
				ELC = 0
	   
				'Get origin
				Origin = ""
				IF INSTR(CurrLine->Value, ";?F") <> 0 THEN 
					Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
					CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
				END IF
				
				'Decide whether to jump to else or end if
				IT = 0
				TL = 1
				FindEnd = CurrLine->Next
				Do While FindEnd <> 0
					Temp = FindEnd->Value
					IF Left(Temp, 3) = "IF " Then TL += 1
					IF Left(Temp, 6) = "END IF" Then TL -= 1
					IF TL = 1 AND Left(Temp, 4) = "ELSE" Then IT = 1: Exit Do
					If TL = 0 Then Exit Do
					FindEnd = FindEnd->Next
				Loop
				'If no end found, error and quit
				If FindEnd = 0 Then
					'Print "No End If"
					LogError(Message("NoEndIf"), Origin)
					Exit Sub
				End If
				
				'Generate code to test and jump
				Condition = Mid(CurrLine->Value, 4)
				Condition = Left(Condition, INSTR(UCase(Condition), "THEN") - 1)   
				NewCode = CompileConditions(Condition, "FALSE", Origin)
				DelSection = 0
				DelEndIf = 0
				If Condition = "AlwaysTrue" Then
					CurrLine = LinkedListDelete(CurrLine)
					DelEndIf = -1
					
				ElseIf Condition = "AlwaysFalse" Then
					DelSection = -1
					StartDel = CurrLine
					
				Else
					CurrLine = LinkedListDelete(CurrLine)
					CurrLine = LinkedListInsertList(CurrLine, NewCode)
					If IT = 0 Then EndBlock = "ENDIF" + Str(ILC)
					If IT = 1 Then EndBlock = "ELSE" + Str(ILC) + "_" + Str(ELC + 1)
					If ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto " + EndBlock)
					If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp " + EndBlock)
					If ModeZ8 Then CurrLine = LinkedListInsert(CurrLine, " jp " + EndBlock)
				End If
					
				FoundIF = -1
			END IF
		
		'Else If
		
		'Else
		ElseIF Left(CurrLine->Value, 4) = "ELSE" And IL = 1 Then
			'Get origin
			Origin = ""
			IF INSTR(CurrLine->Value, ";?F") <> 0 THEN 
				Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
				CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
			END IF
			
			'Add Code
			If DelEndIf Then
				StartDel = CurrLine
				DelSection = -1
				DelEndIf = 0
			ElseIf DelSection Then
				'DelLines StartDel, CD
				'CD = StartDel - 1
				CurrLine = LinkedListDeleteList(StartDel, CurrLine)
				DelSection = 0
				DelEndIf = -1
			Else
				If ModePIC Then
					L1 = " goto ENDIF" + Str(ILC)
					L2 = "ELSE" + Str(ILC) + "_" + Str(ELC + 1)
				
				ElseIf ModeAVR Then
					L1 = " rjmp ENDIF" + Str(ILC)
					L2 = "ELSE" + Str(ILC) + "_" + Str(ELC + 1) + ":"
					
				ElseIf ModeZ8 Then
					L1 = " jp ENDIF" + Str(ILC)
					L2 = "ELSE" + Str(ILC) + "_" + Str(ELC + 1) + ":"
				End If
				CurrLine->Value = L1
				CurrLine = LinkedListInsert(CurrLine, L2)
			End If
		
		'End If
		ElseIF UCase(Left(CurrLine->Value, 6)) = "END IF" THEN
			'Get origin
			Origin = ""
			IF INSTR(CurrLine->Value, ";?F") <> 0 THEN 
				Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
				CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?F") - 1))
			END IF
	  
			'Decrement level counter, add end if label if needed
			IL = IL - 1
			IF IL = 0 THEN
				If DelEndIf Then
					CurrLine = LinkedListDelete(CurrLine)
				ElseIf DelSection Then
					'DelLines StartDel, CD
					'CD = StartDel - 1
					CurrLine = LinkedListDeleteList(StartDel, CurrLine)
				Else
					CurrLine->Value = "ENDIF" + Str(ILC) + LabelEnd
				End If
			End If
			
			'If there are too many END IFs, display error
			IF IL < 0 THEN
				IF Origin <> "" THEN
					LogError Message("ExtraENDIF"), Origin
				END IF
				IL = 0
			END IF
		
		END IF
		
		CurrLine = CurrLine->Next
	Loop
	
	'Need to scan through program over and over until no more new IFs are found
	IF FoundIF THEN GOTO COMPILEIFS
	
	FoundCount = ILC
END SUB

Sub CompileIntOnOff (CompSub As SubType Pointer)
	Dim As String LineTemp, TempData, Origin
	Dim NewCode(10) As String
	Dim As LinkedListElement Pointer CurrLine
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		LineTemp = CurrLine->Value
		IF LEFT(LineTemp, 5) = "INTON" Or LEFT(LineTemp, 6) = "INTOFF" THEN
			
			Origin = ""
			IF INSTR(LineTemp, ";?F") <> 0 THEN 
				Origin = Mid(LineTemp, INSTR(LineTemp, ";?F"))
				LineTemp = RTRIM(LEFT(LineTemp, INSTR(LineTemp, ";?F") - 1))
			END IF
			If LineTemp <> "INTON" And LineTemp <> "INTOFF" Then Continue Do
			
			If SysInt Or UserInt Then
				CurrLine = LinkedListDelete(CurrLine)
				
				If ModePIC Then
					If LineTemp = "INTON" Then
						'Check if SysIntOffCount is 0
						CurrLine = LinkedListInsert(CurrLine, " movf SysIntOffCount,F")
						'If not 0, dec
						CurrLine = LinkedListInsert(CurrLine, " btfss STATUS,Z")
						CurrLine = LinkedListInsert(CurrLine, " decf SysIntOffCount,F")
						'If is 0, enable int
						CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
						CurrLine = LinkedListInsert(CurrLine, " bsf INTCON,GIE")
						
					Else
						CurrLine = LinkedListInsert(CurrLine, " bcf INTCON,GIE")
						CurrLine = LinkedListInsert(CurrLine, " incf SysIntOffCount,F")
						
					End If
					
				ElseIf ModeAVR Then
					If LineTemp = "INTON" Then
						CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy,SysIntOffCount")
						CurrLine = LinkedListInsert(CurrLine, " cpi SysValueCopy, 0")
						CurrLine = LinkedListInsert(CurrLine, " breq PC + 4")
						CurrLine = LinkedListInsert(CurrLine, " dec SysValueCopy")
						CurrLine = LinkedListInsert(CurrLine, " sts SysIntOffCount,SysValueCopy")
						CurrLine = LinkedListInsert(CurrLine, " brne PC + 2")
						CurrLine = LinkedListInsert(CurrLine, " sei")
					Else
						CurrLine = LinkedListInsert(CurrLine, " cli")
						CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy,SysIntOffCount")
						CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
						CurrLine = LinkedListInsert(CurrLine, " sts SysIntOffCount,SysValueCopy")
					End If
					
				End If
				
			Else
				'If no user or system interrupt routine, delete IntOn and IntOff
				CurrLine = LinkedListDelete(CurrLine)
				
			End If
		End If
		
		CurrLine = CurrLine->Next
	Loop
	
End Sub

SUB CompileOn (CompSub As SubType Pointer)
	Dim As String LineTemp, Origin, TempData, EnableBit, FlagBit
	Dim As String OnType, OnCondition, OnJumpTo
	Dim As String IntSource, IntEvent
	Dim As Integer IntIndex, FindIndex, HandlerSubLoc
	Dim As LinkedListElement Pointer CurrLine, OutLine
	FoundCount = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		LineTemp = CurrLine->Value
		IF Left(LineTemp, 3) = "ON " THEN
			
			Origin = ""
			IF INSTR(LineTemp, ";?F") <> 0 THEN 
				Origin = Mid(LineTemp, INSTR(LineTemp, ";?F"))
				LineTemp = RTRIM(LEFT(LineTemp, INSTR(LineTemp, ";?F") - 1))
			END IF
			
			'Get condition, type
			OnType = Mid(LineTemp, INSTR(LineTemp, " ") + 1)
			OnCondition = Trim(Mid(OnType, INSTR(OnType, " ") + 1))
			OnType = Trim(Left(OnType, INSTR(OnType, " ") - 1))
			
			'Get target
			If Instr(OnCondition, " CALL ") <> 0 Then
				OnJumpTo = Trim(Mid(OnCondition, Instr(OnCondition, " CALL ") + 6))
				OnCondition = Trim(Left(OnCondition, Instr(OnCondition, " CALL ") - 1))
				
			ElseIf Instr(OnCondition, " GOSUB ") <> 0 Then
				OnJumpTo = Trim(Mid(OnCondition, Instr(OnCondition, " GOSUB ") + 7))
				OnCondition = Trim(Left(OnCondition, Instr(OnCondition, " GOSUB ") - 1))
				
			ElseIf Instr(OnCondition, " IGNORE") <> 0 Then
				OnJumpTo = "IGNORE"
				OnCondition = Trim(Left(OnCondition, Instr(OnCondition, " IGNORE") - 1))
				
			Else
				'Error, no target
				LogError Message("MissingTarget"), Origin
				CurrLine = LinkedListDelete(CurrLine)
				Continue Do
			End If
			
			'On Error ...
			If OnType = "ERROR" Then
				'Implement in future version
				
			'On Interrupt
			ElseIf OnType = "INTERRUPT" Then
				AllowInterrupt = -1
				SysInt = -1
				
				'Get index
				IntIndex = 0
				FOR FindIndex = 1 to IntCount
					If UCase(Interrupts(FindIndex).EventName) = OnCondition Then IntIndex = FindIndex: Exit For
				Next
				If IntIndex = 0 Then
					TempData = Message("BadIntEvent")
					Replace TempData, "%event%", OnCondition
					LogError TempData, Origin
					CurrLine = LinkedListDelete(CurrLine)
					Continue Do
				End If
					
				With Interrupts(IntIndex)
					
					'Show error if conflicting handler found
					If .Handler <> "" And .Handler <> OnJumpTo And OnJumpTo <> "IGNORE" Then
						TempData = Message("HandlerConflict")
						Replace TempData, "%event%", OnCondition
						LogError TempData, Origin
						
						CurrLine = LinkedListDelete(CurrLine)
						Continue Do
					End If
					
					'Check handler sub, request
					If OnJumpTo <> "IGNORE" Then
						HandlerSubLoc = RequestSub(0, OnJumpTo)
						If HandlerSubLoc = -1 Then
							'Error, handler sub not found
							TempData = Message("SubNotFound")
							Replace TempData, "%sub%", OnJumpTo
							LogError TempData, Origin
							
							CurrLine = LinkedListDelete(CurrLine)
							Continue Do
						End If
					End If
					
					'On PIC, generate handler routine in IntCode()
					If ModePIC Then
						If OnJumpTo = "IGNORE" Then
							If .EnableBit <> "" Then
								CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 0"
							Else
								CurrLine = LinkedListDelete(CurrLine)
							End If
						Else
							If .EnableBit <> "" Then
								EnableBit = GetWholeSFR(.EnableBit)
								FlagBit = GetWholeSFR(.FlagBit)
								Replace EnableBit, ".", ","
								Replace FlagBit, ".", ","
								
								CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 1"
								If .Handler = "" Then
									If .FlagBit = "" Then
										CurrLine = LinkedListDelete(CurrLine)
										TempData = Message("WarningNoIntFlag")
										Replace TempData, "%event%", .EventName
										LogWarning TempData, Origin
									Else
										OutLine = IntHandlerCode->CodeEnd
										OutLine = LinkedListInsert(OutLine, " btfss " + EnableBit)
										OutLine = LinkedListInsert(OutLine, " goto Not" + UCase(.FlagBit))
										OutLine = LinkedListInsert(OutLine, " btfss " + FlagBit)
										OutLine = LinkedListInsert(OutLine, " goto Not" + UCase(.FlagBit))
										OutLine = LinkedListInsert(OutLine, " call " + OnJumpTo)
										OutLine = LinkedListInsert(OutLine, " bcf " + FlagBit)
										OutLine = LinkedListInsert(OutLine, " goto INTERRUPTDONE")
										OutLine = LinkedListInsert(OutLine, "Not" + UCase(.FlagBit))
										IntHandlerCode->CodeEnd = OutLine
									End If
									.Handler = OnJumpTo
								End If
							Else
								CurrLine = LinkedListDelete(CurrLine)
								TempData = Message("WarningNoIntEnable")
								Replace TempData, "%event%", .EventName
								LogWarning TempData, Origin
							End If
							
						End If
						
					'On AVR, set Interrupt().Handler
					ElseIf ModeAVR Then
						If OnJumpTo = "IGNORE" Then
							'Prog(PD) = "SET " + GetWholeSFR(.EnableBit) + " 0"
							If .EnableBit <> "" Then
								CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 0"
								If .FlagBit = "" Then
									CurrLine = LinkedListDelete(CurrLine)
									TempData = Message("WarningNoIntFlag")
									Replace TempData, "%event%", .EventName
									LogWarning TempData, Origin
								End If
							Else
								CurrLine = LinkedListDelete(CurrLine)
							End If
						Else
							'Prog(PD) = "SET " + GetWholeSFR(.EnableBit) + " 1"
							If .EnableBit <> "" Then
								CurrLine->Value = "SET " + GetWholeSFR(.EnableBit) + " 1"
							Else
								CurrLine = LinkedListDelete(CurrLine)
								TempData = Message("WarningNoIntEnable")
								Replace TempData, "%event%", .EventName
								LogWarning TempData, Origin
							End If
							.Handler = OnJumpTo
						End If
						
					End If
				End With
				
			'Error, bad type
			Else
				TempData = Message("MissingTarget")
				Replace TempData, "%found%", OnType
				LogError TempData, Origin
				Continue Do
			End If
			
			FoundCount = FoundCount + 1
		END If
		
		CurrLine = CurrLine->Next
	Loop
	
END SUB

SUB CompilePot (CompSub As SubType Pointer)
	FoundCount = 0
	
	Dim As String LineTemp, Port, OutVar, Origin
	Dim As LinkedListElement Pointer CurrLine
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		LineTemp = CurrLine->Value
		IF Left(LineTemp, 4) = "POT " THEN
			Port = Mid(LineTemp, 5)
			
			Origin = ""
			IF INSTR(Port, ";?F") <> 0 THEN 
				Origin = Mid(Port, INSTR(Port, ";?F"))
				Port = RTrim(Left(Port, INSTR(Port, ";?F") - 1))
			END IF
			
			OutVar = LTRIM(Mid(Port, INSTR(Port, ",") + 1))
			Port = Trim(Left(Port, INSTR(Port, ",") - 1))
			POC = POC + 1
			
			CurrLine = LinkedListDelete(CurrLine)
			CurrLine = LinkedListInsert(CurrLine, "DIR " + Port + " OUT" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "SET " + Port + " 0" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "WAIT 5 MS" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "INTOFF")
			CurrLine = LinkedListInsert(CurrLine, "DIR " + Port + " IN" + Origin)
			CurrLine = LinkedListInsert(CurrLine, OutVar + "=0" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "DO WHILE " + Port + " = 0" + Origin)
			CurrLine = LinkedListInsert(CurrLine, OutVar + "=" + OutVar + "+1" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "IF " + OutVar + " = 0 THEN" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "GOTO ENDPOT" + Str(POC) + Origin)
			CurrLine = LinkedListInsert(CurrLine, "END IF" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "WAIT 10 10US" + Origin)
			CurrLine = LinkedListInsert(CurrLine, "LOOP " + Origin)
			CurrLine = LinkedListInsert(CurrLine, "ENDPOT" + Str(POC) + ":")
			CurrLine = LinkedListInsert(CurrLine, "INTON")
			
			FoundCount += 1
			
		END If
		
		CurrLine = CurrLine->Next
	Loop
	
END SUB

SUB CompileReadTable (CompSub As SubType Pointer)
	Dim As String InLine, Origin, TableName, TableLoc, OutVar, Temp
	Dim As Integer CD, TableID, TableBytes, OutBytes, CurrTableByte, ItemLoc, ItemVal
	Dim As LinkedListElement Pointer CurrLine
	Dim As PinDirType Pointer CurrPinDir
	FoundCount = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = CurrLine->Value
 		IF Left(InLine, 10) = "READTABLE " THEN
			
			'Get origin of line
  			Origin = ""
  			IF INSTR(InLine, ";?F") <> 0 THEN 
   				Origin = Mid(InLine, INSTR(InLine, ";?F"))
   				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
  			END IF
			
  			'Get table name
  			InLine = LTrim(Mid(InLine, 11))
  			TableName = Trim(Left(InLine, INSTR(InLine, ",") - 1))
  			InLine = Mid(InLine, INSTR(InLine, ",") + 1)
			
  			'Get table location
  			TableLoc = Trim(Left(InLine, INSTR(InLine, ",") - 1))
  			InLine = Mid(InLine, INSTR(InLine, ",") + 1)
			
  			'Get output variable
  			OutVar = Trim(InLine)
  			AddVar OutVar, "BYTE", 1, CompSub, "REAL", Origin
			
  			'Check that table exists
  			TableID = 0
  			FOR CD = 1 to DataTables
				IF DataTable(CD).Name = UCase(Trim(TableName)) Then
					TableID = CD 
					Exit For
				End If
  			NEXT
  			
  			'Table not found, show error
  			IF TableID = 0 THEN
				Temp = Message("TableNotFound")
				Replace Temp, "%Table%", TableName
				LogError Temp, Origin
  				CurrLine = LinkedListDelete(CurrLine)
  				
  			'Table found, continue compile
  			Else
	  			'Get size of data in table
	  			TableBytes = GetTypeSize(DataTable(TableID).Type)
	  			OutBytes = GetTypeSize(TypeOfVar(OutVar, CompSub))
	  			
	  			'Remove input line
	  			CurrLine = LinkedListDelete(CurrLine)
	  			
	  			'Write assembly
	  			'If reading from fixed location, remove call to table
	  			If IsConst(TableLoc) Then
	  				ItemLoc = MakeDec(TableLoc)
	  				With DataTable(TableID)
	  					If ItemLoc = 0 Then
		  					ItemVal = .Items
	  					ElseIf ItemLoc <= .Items Then
	  						ItemVal = .Item(ItemLoc)
	  					Else
	  						ItemVal = 0
	  					End If
	  					
	  					CurrLine = LinkedListInsert(CurrLine, OutVar + "=" + Str(ItemVal) + Origin)
	  				End With
	  			Else
		  			'Request table
					DataTable(TableID).Used = -1
					
		  			'Pseudo code:
		  			' movf/movlw TableLoc
		  			' call TableName
		  			' movwf OutVar
					If ModePIC Then
		  				If DataTable(TableID).StoreLoc = 0 Then
			  				'Store in program memory
			  				For CurrTableByte = 1 To OutBytes
			  					If CurrTableByte > TableBytes Then
			  						CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(OutVar, CurrTableByte - 1))
			  					Else
									CurrLine = LinkedListInsert(CurrLine, "DataPointer=" + TableLoc)
					  				CurrLine = LinkedListInsert(CurrLine, " call " + GetByte(TableName, CurrTableByte - 1))
					  				CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(OutVar, CurrTableByte - 1))
			  					End If
			  				Next
			  				
			  				'If writing to a port, record it
							CurrPinDir = GetPinDirection(OutVar)
							If CurrPinDir <> 0 Then
								CurrPinDir->WrittenTo = -1
							End If
			  				
		  				ElseIf DataTable(TableID).StoreLoc = 1 Then
		  					'Store in EEPROM
		  					RequestSub(CompSub, "SysEPRead")
		  					CurrLine = LinkedListInsert(CurrLine, "EEAddress=" + TableLoc + "+@Table" + TableName)
		  					CurrLine = LinkedListInsert(CurrLine, " call SysEPRead")
		  					CurrLine = LinkedListInsert(CurrLine, OutVar + "=EEDataValue")
		  				End If
		  				
					ElseIf ModeAVR Then
			  			If DataTable(TableID).StoreLoc = 0 Then
			  				'Store in program memory
			  				For CurrTableByte = 1 To OutBytes
			  					If CurrTableByte > TableBytes Then
			  						CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("0", "[BYTE]" + GetByte(OutVar, CurrTableByte - 1), Origin))
			  					Else
					  				CurrLine = LinkedListInsert(CurrLine, "SysValueCopy=" + TableLoc)
					  				CurrLine = LinkedListInsert(CurrLine, " call " + GetByte(TableName, CurrTableByte - 1))
					  				CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysCalcTempX", "[BYTE]" + GetByte(OutVar, CurrTableByte - 1), Origin))
			  					End If
			  				Next
			  				
			  			ElseIf DataTable(TableID).StoreLoc = 1 Then
		  					'Store in EEPROM
		  					RequestSub(CompSub, "SysEPRead")
		  					CurrLine = LinkedListInsert(CurrLine, "EEAddress=" + TableLoc + "+@Table" + TableName)
		  					CurrLine = LinkedListInsert(CurrLine, " call SysEPRead")
		  					CurrLine = LinkedListInsert(CurrLine, OutVar + "=EEDataValue")
			  			End If
		  				
					End If
	  			End If
  			END IF
				
 		End IF
		
		CurrLine = CurrLine->Next
	Loop
END SUB

SUB CompileRepeat (CompSub As SubType Pointer)
	
	Dim As String InLine, Origin, Temp, RepCount, NewOrigin, RepValType
	Dim As Integer RVN, RL, RepWord, EV, FE, FS, RepNone, CheckZero
	Dim As LinkedListElement Pointer CurrLine, EndLoc, FindEnd
	
	'Was RVN = 0
	'Quick and dirty fix to stop variables getting reused badly
	'set initial var number to number of repeat loops
	RVN = RPLC
	FoundCount = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = CurrLine->Value
		RL = 0
		
		IF Left(InLine, 7) = "REPEAT " THEN
			
			'Get origin
			Origin = ""
			NewOrigin = ";?F0L0"
			IF INSTR(InLine, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
				NewOrigin = Left(Origin, InStr(Origin, "S") - 1)
			END IF
			For FS = 0 To SBC
				If Subroutine(FS) = CompSub Then
					NewOrigin += "S" + Str(FS) + "?"
					Exit For
				End If
			Next
			
			'Get parameter
			'Note 10/8/2010 - what on earth was I thinking when I wrote this?
			'Replace InLine, "(", " "
			'Replace InLine, ")", " "
			'RepCount = Trim(Mid(InLine, INSTR(InLine, " ") + 1))
			
			RepCount = Trim(Mid(InLine, 7))
			If RepCount = "" Then
				LogError Message("RepeatMissingCount"), Origin
			End If
			
			RepWord = 0
			'IF Not IsConst(RepCount) THEN
			'	IF IsWord(RepCount, GetSubID(Origin)) THEN RepWord = -1
			'END IF
			'IF MakeDec(RepCount) > 255 THEN RepWord = -1
			RepValType = TypeOfValue(RepCount, CompSub) 
			If RepValType = "WORD" Or RepValType = "INTEGER" Then RepWord = -1
			
			RepNone = 0
			CheckZero = -1
			
			'Is count zero?
			If IsConst(RepCount) Then
				CheckZero = 0
				If MakeDec(RepCount) = 0 Then
					RepNone = -1
				End If
			End If
			
			RL = 1
			If RepNone = 0 Then
				RPLC += 1
				RVN += 1
				If RepWord THEN
					AddVar "SysRepeatTemp" + Str(RVN), "WORD", 1, CompSub, "REAL", Origin
				Else
					AddVar "SysRepeatTemp" + Str(RVN), "BYTE", 1, CompSub, "REAL", Origin
				End If
			End If
			
			'Pseudo code:
			'V = RV
			'SysRepeatLoop(n):
			'...
			'decfsz RV, F
			'goto SysRepeatLoop(n)
			
			'Starting code
			If RepNone = 0 Then
				CurrLine = LinkedListDelete(CurrLine)
				If RepWord Then
					CurrLine = LinkedListInsert(CurrLine, "SysRepeatTemp" + Str(RVN) + "=" + RepCount + "+256" + NewOrigin)
				Else
					CurrLine = LinkedListInsert(CurrLine, "[byte]SysRepeatTemp" + Str(RVN) + "=" + RepCount + NewOrigin)
				End If
				
				'Check for zero, don't enter loop if 0 found
				If CheckZero Then
					If ModePIC And Not RepWord And ChipFamily <> 16 Then
						'Can't use this on 18F, movff is used for copy, which doesn't set Z
						CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
					Else
						If RepWord Then
							CurrLine = LinkedListInsertList(CurrLine, CompileConditions("SysRepeatTemp" + Str(RVN) + "<1", "TRUE", NewOrigin))
						Else
							CurrLine = LinkedListInsertList(CurrLine, CompileConditions("[byte]SysRepeatTemp" + Str(RVN) + "=0", "TRUE", NewOrigin))
						End If
					End If
					If ModePIC Then
						CurrLine = LinkedListInsert(CurrLine, " goto SysRepeatLoopEnd" + Str(RPLC))
					ElseIf ModeAVR Then
						CurrLine = LinkedListInsert(CurrLine, " jmp SysRepeatLoopEnd" + Str(RPLC))
					End If
				End If
				
				CurrLine = LinkedListInsert(CurrLine, "SysRepeatLoop" + Str(RPLC) + LabelEnd)
			End If
			
			'Find End
			EndLoc = 0
			FindEnd = CurrLine->Next
			Do While FindEnd <> 0
				IF Left(FindEnd->Value, 7) = "REPEAT " THEN RL += 1
				IF Left(FindEnd->Value, 10) = "END REPEAT" THEN RL -= 1
				IF RL = 0 THEN EndLoc = FindEnd: Exit Do
				FindEnd = FindEnd->Next
			Loop
			'Match not found error
			IF EndLoc = 0 THEN
				LogError Message("NoEndRepeat"), Origin
				Goto NextRepeat
			END IF
			
			'Add end code
			If RepNone Then
				'If no repeating, delete entire loop
				CurrLine = LinkedListDeleteList(CurrLine, EndLoc)
				
			Else
				EndLoc = LinkedListDelete(EndLoc)
				If ModePIC Then
					EndLoc = LinkedListInsert(EndLoc, " decfsz SysRepeatTemp" + Str(RVN) + ",F")
					EndLoc = LinkedListInsert(EndLoc, " goto SysRepeatLoop" + Str(RPLC))
					If RepWord THEN
						EndLoc = LinkedListInsert(EndLoc, " decfsz SysRepeatTemp" + Str(RVN) + "_H,F")
						EndLoc = LinkedListInsert(EndLoc, " goto SysRepeatLoop" + Str(RPLC))
					END If
					
				ElseIf ModeAVR Then
					Dim As String RegName
					Dim As LinkedListElement Pointer CopyToReg
					CopyToReg = LinkedListCreate
					If RepWord Then
						RegName = PutInRegister(CopyToReg, "[word]SysRepeatTemp" + Str(RVN), "WORD", Origin)
					Else
						RegName = PutInRegister(CopyToReg, "[byte]SysRepeatTemp" + Str(RVN), "", Origin)
					End If
					EndLoc = LinkedListInsertList(EndLoc, CopyToReg)
					EndLoc = LinkedListInsert(EndLoc, " dec " + GetByte(RegName, 0))
					EndLoc = LinkedListInsertList(EndLoc, CompileVarSet("[byte]" + GetByte(RegName, 0), "[byte]SysRepeatTemp" + Str(RVN), Origin))
					EndLoc = LinkedListInsert(EndLoc, " breq PC + 2")
					EndLoc = LinkedListInsert(EndLoc, " rjmp SysRepeatLoop" + Str(RPLC))
					If RepWord Then
						EndLoc = LinkedListInsert(EndLoc, " dec " + GetByte(RegName, 1))
						EndLoc = LinkedListInsertList(EndLoc, CompileVarSet("[byte]" + GetByte(RegName, 1), "[byte]SysRepeatTemp" + Str(RVN) + "_H", Origin))
						EndLoc = LinkedListInsert(EndLoc, " breq PC + 2")
						EndLoc = LinkedListInsert(EndLoc, " rjmp SysRepeatLoop" + Str(RPLC))
					End If
					FreeCalcVar RegName
				End If
				EndLoc = LinkedListInsert(EndLoc, "SysRepeatLoopEnd" + Str(RPLC) + LabelEnd)
			End If
			
		End IF
		
		IF ModePIC And (INSTR(InLine, " decfsz SysRepeatTemp") <> 0 AND INSTR(InLine, "_H") = 0) Then RVN = RVN - 1
		If ModeAVR And (INSTR(InLine, " sts SysRepeatTemp") <> 0 AND INSTR(InLine, "_H") = 0) Then RVN = RVN - 1
		
		NextRepeat:
		CurrLine = CurrLine->Next
	Loop
	FoundCount = RPLC
END SUB

SUB CompileRotate (CompSub As SubType Pointer)
	
	FoundCount = 0
	Dim As String InLine, Origin, Temp, VarName, VarType, Direction, RotateReg
	Dim As Integer UseC, OldType
	Dim As LinkedListElement Pointer CurrLine
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = UCase(CurrLine->Value)
		IF Left(InLine, 7) = "ROTATE " THEN
			
			'Get origin
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN 
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			'Get var, var type, direction, mode
			VarName = Mid(InLine, INSTR(InLine, " ") + 1)
			Direction = Mid(VarName, INSTR(VarName, " ") + 1)
			VarName = Left(VarName, INSTR(VarName, " ") - 1)
			VarType = TypeOfVar(VarName, Subroutine(GetSubID(Origin)))
			IF Direction = "LC" Then Direction = "LEFT"
			IF Direction = "RC" Then Direction = "RIGHT"
			IF Direction = "L" Then Direction = "LEFT SIMPLE"
			IF Direction = "R" Then Direction = "RIGHT SIMPLE"
			
			'Check var type
			IF VarType = "INTEGER" THEN VarType = "WORD" 'Treated identically by this command
			If VarType = "SINGLE" Or VarType = "STRING" Then 'Error
				Temp = Message("BadCommandType")
				Replace Temp, "%command%", "Rotate"
				Replace Temp, "%type%", VarType
				LogError Temp, Origin
				Goto CompileNextRotate
			End If
			
			'Use carry flag?
			UseC = -1
			IF INSTR(Direction, "SIMPLE") <> 0 Then
				UseC = 0
				Replace Direction, "SIMPLE", ""
				Direction = TRIM(Direction)
			End If
			
			'Delete BASIC command, makes it a bit neater below
			CurrLine = LinkedListDelete(CurrLine)
			
			'Put into register
			If ModeAVR Then
				If IsRegister(VarName) Then
					RotateReg = VarName
					OldType = 0
				ElseIf IsLowIOReg(VarName) Then
					CurrLine = LinkedListInsert(CurrLine, "in SysCalcTempA," + VarName)
					RotateReg = "SysCalcTempA"
					OldType = 1
				Else
					CurrLine = LinkedListInsert(CurrLine, "lds SysCalcTempA," + VarName)
					If VarType = "WORD" Then
						CurrLine = LinkedListInsert(CurrLine, "lds SysCalcTempA_H," + VarName + "_H")
					ElseIf VarType = "LONG" Then
						CurrLine = LinkedListInsert(CurrLine, "lds SysCalcTempA_H," + VarName + "_H")
						CurrLine = LinkedListInsert(CurrLine, "lds SysCalcTempA_U," + VarName + "_U")
						CurrLine = LinkedListInsert(CurrLine, "lds SysCalcTempA_E," + VarName + "_E")
					End If
					RotateReg = "SysCalcTempA"
					OldType = 2
				End If
			End If
			
			'Save carry
			If (ChipFamily <> 16 OR VarType = "WORD") And Not UseC Then
				If ModePIC Then
					If Direction = "RIGHT" Then CurrLine = LinkedListInsert(CurrLine, " rrf " + VarName + ",W")
					If Direction = "LEFT" Then
						Select Case VarType
							Case "BYTE": CurrLine = LinkedListInsert(CurrLine, " rlf " + VarName + ",W")
							Case "WORD": CurrLine = LinkedListInsert(CurrLine, " rlf " + VarName + "_H,W")
							Case "LONG": CurrLine = LinkedListInsert(CurrLine, " rlf " + VarName + "_E,W")
						End Select
					End If
					
				ElseIf ModeAVR Then
					CurrLine = LinkedListInsert(CurrLine, " clc")
					If Direction = "RIGHT" Then CurrLine = LinkedListInsert(CurrLine, " sbrc " + RotateReg + ",0")
					If Direction = "LEFT" Then
						Select Case VarType
							Case "BYTE": CurrLine = LinkedListInsert(CurrLine, " sbrc " + RotateReg + ",7")
							Case "WORD": CurrLine = LinkedListInsert(CurrLine, " sbrc " + RotateReg + "_H,7")
							Case "LONG": CurrLine = LinkedListInsert(CurrLine, " sbrc " + RotateReg + "_E,7")
						End Select
					End If
					CurrLine = LinkedListInsert(CurrLine, " sec")
				End If
			End If
			
			'Rotate high byte/s (right only)
			If Direction = "RIGHT" THEN
				If VarType = "LONG" Then
					If ModePIC Then
						If ChipFamily <> 16 Then
							CurrLine = LinkedListInsert(CurrLine, " rrf " + VarName + "_E,F")
							CurrLine = LinkedListInsert(CurrLine, " rrf " + VarName + "_U,F")
						Else
							CurrLine = LinkedListInsert(CurrLine, " rrcf " + VarName + "_E,F")
							CurrLine = LinkedListInsert(CurrLine, " rrcf " + VarName + "_U,F")
						End If
					ElseIf ModeAVR Then
						CurrLine = LinkedListInsert(CurrLine, " ror " + RotateReg + "_E")
						CurrLine = LinkedListInsert(CurrLine, " ror " + RotateReg + "_U")
					End If
				End If
				
				If VarType = "WORD" Or VarType = "LONG" Then
					If ModePIC Then
						If ChipFamily <> 16 THEN CurrLine = LinkedListInsert(CurrLine, " rrf " + VarName + "_H,F")
						If ChipFamily = 16 THEN CurrLine = LinkedListInsert(CurrLine, " rrcf " + VarName + "_H,F")
					ElseIf ModeAVR Then
						CurrLine = LinkedListInsert(CurrLine, " ror " + RotateReg + "_H")
					End If
				End If
			END IF
			
			'Rotate low byte
			If ModePIC Then
				IF ChipFamily <> 16 THEN
					Temp = " rrf "
					IF Direction = "LEFT" THEN Temp = " rlf "
				Else
					If UseC OR VarType = "WORD" Then Temp = " rrcf ": IF Direction = "LEFT" THEN Temp = " rlcf "
					If Not UseC AND VarType <> "WORD" Then Temp = " rrncf ": IF Direction = "LEFT" THEN Temp = " rlncf "
				END IF
				CurrLine = LinkedListInsert(CurrLine, Temp + VarName + ",F")
				IF VarType <> "WORD" THEN AddVar VarName, "BYTE", 1, 0, "REAL", Origin
			ElseIf ModeAVR Then
				Temp = " ror "
				IF Direction = "LEFT" THEN Temp = " rol "
				CurrLine = LinkedListInsert(CurrLine, Temp + RotateReg)
			End If
			
			'Rotate high byte/s (left only)
			If Direction = "LEFT" THEN
				If VarType = "WORD" Or VarType = "LONG" Then
					If ModePIC Then
						IF ChipFamily <> 16 THEN CurrLine = LinkedListInsert(CurrLine, " rlf " + VarName + "_H,F")
						IF ChipFamily = 16 THEN CurrLine = LinkedListInsert(CurrLine, " rlcf " + VarName + "_H,F")
					ElseIf ModeAVR Then
						CurrLine = LinkedListInsert(CurrLine, " rol " + RotateReg + "_H")
					End If
				End If
				
				If VarType = "LONG" Then
					If ModePIC Then
						IF ChipFamily <> 16 Then
							CurrLine = LinkedListInsert(CurrLine, " rlf " + VarName + "_U,F")
							CurrLine = LinkedListInsert(CurrLine, " rlf " + VarName + "_E,F")
						Else
							CurrLine = LinkedListInsert(CurrLine, " rlcf " + VarName + "_U,F")
							CurrLine = LinkedListInsert(CurrLine, " rlcf " + VarName + "_E,F")
						End If
					ElseIf ModeAVR Then
						CurrLine = LinkedListInsert(CurrLine, " rol " + RotateReg + "_U")
						CurrLine = LinkedListInsert(CurrLine, " rol " + RotateReg + "_E")
					End If
				End If
			END IF
			
			'Put variable back in original location (AVR only)
			If ModeAVR Then
				'IO register
				If OldType = 1 Then
					CurrLine = LinkedListInsert(CurrLine, " out " + VarName + ",SysCalcTempA")
					
				'SRAM variable
				ElseIf OldType = 2 Then
					CurrLine = LinkedListInsert(CurrLine, " sts " + VarName + ",SysCalcTempA")
					IF VarType = "WORD" Or VarType = "LONG" Then
						CurrLine = LinkedListInsert(CurrLine, " sts " + VarName + "_H,SysCalcTempA_H")
					End If
					If VarType = "LONG" Then
						CurrLine = LinkedListInsert(CurrLine, " sts " + VarName + "_U,SysCalcTempA_U")
						CurrLine = LinkedListInsert(CurrLine, " sts " + VarName + "_E,SysCalcTempA_E")
					End If
					IF VarType = "BYTE" THEN AddVar VarName, "BYTE", 1, 0, "REAL", Origin
				End If
			End If
			
			FoundCount = FoundCount + 1
		END IF
	
		CompileNextRotate:
		CurrLine = CurrLine->Next
	Loop
	
END SUB

Function CompileString (InLine As String, Origin As String) As LinkedListElement Pointer
	Dim As String DestVar, Temp, ArrayName, StringToCopy
	Dim As String Source, SourceType, SourceTable, DestArrayName
	Dim As Integer ArrayType, CD, TC, BL, SS, SArrayType, ST, CurrSubID
	Dim As VariableType Pointer SourcePtr
	
	Dim StringTemp(100) As String
	
	Dim As LinkedListElement Pointer OutList, CurrLine
	Dim As SubType Pointer CurrSub
	
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Find subroutine that contains array
	CurrSubID = GetSubID(Origin)
	CurrSub = Subroutine(CurrSubID)
		
	'Disable interrupt
	'CurrLine = LinkedListInsert(CurrLine, "INTOFF")
	
	'Get destination array
	DestVar = Trim(Left(InLine, INSTR(InLine, "=") - 1))
	ArrayName = DestVar
	If INSTR(DestVar, "(") <> 0 Then ArrayName = RTrim(Left(DestVar, INSTR(DestVar, "(") - 1))
	If INSTR(DestVar, "$") <> 0 Then ArrayName = RTrim(Left(DestVar, INSTR(DestVar, "$") - 1))
	DestArrayName = ArrayName
	
	'Add code to set destination array RAM pointer
	CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, 1, CurrSub, Origin))
	
	'Get string to copy
	StringToCopy = Trim(Mid(InLine, INSTR(InLine, "=") + 1))
	TC = 0
	BL = 0
	Temp = ""
	For SS = 1 TO Len(StringToCopy)
		Select Case Mid(StringToCopy, SS, 1)
		Case "(":
			BL += 1
		Case ")":
			BL -= 1
		Case "+":
			If BL = 0 Then
				TC += 1: StringTemp(TC) = Trim(Temp)
				Temp = ""
			Else
				Temp = Temp + "+"
			End If
		Case Else:
			Temp = Temp + Mid(StringToCopy, SS, 1)
		End Select
	Next
	TC += 1: StringTemp(TC) = Trim(Temp)

	'Add code to clear length counter
	If TC > 1 Then
		If ModePIC Then
			CurrLine = LinkedListInsert(CurrLine, " clrf SysStringLength")
		ElseIf ModeAVR Then
			CurrLine = LinkedListInsert(CurrLine, " clr SysStringLength")
			CurrLine = LinkedListInsert(CurrLine, " ld SysValueCopy, Y+") 'Need to pre-increment Y pointer
		End If
		AddVar "SysStringLength", "BYTE", 1, 0, "REAL", Origin
	End If
	
	'Set string
	FOR CD = 1 to TC
		Source = StringTemp(CD)
		
		'Get source type
		SourceType = TypeOfValue(Source, Subroutine(GetSubID(Origin)))
		If IsConst(Source) Then SourceType = "CONST"
		IF INSTR(Source, ";STRING") <> 0 THEN SourceType = "SCONST"
		'Color 14: Print SourceType: Color 7
		Select Case SourceType
		
		'String
		Case "STRING":
			CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(Source, 0, CurrSub, Origin))
			RequestSub(CurrSub, "SysCopyString")
			IF TC = 1 Then CurrLine = LinkedListInsert(CurrLine, " call SysCopyString")
			IF TC > 1 Then CurrLine = LinkedListInsert(CurrLine, " call SysCopyStringPart")
		
		'String const
		Case "SCONST":
			'Add code to set ROM pointer
			ST = VAL(Mid(Source, 8))
			SourceTable = "StringTable" + Str(ST)
			StringStore(ST).Used = -1
			If ModePIC Then
				If ChipFamily <> 16 Then
					AddVar "SysStringA", "WORD", 1, 0, "REAL", Origin
					CurrLine = LinkedListInsert(CurrLine, " movlw low " + SourceTable)
					CurrLine = LinkedListInsert(CurrLine, " movwf SysStringA")
					CurrLine = LinkedListInsert(CurrLine, " movlw high " + SourceTable)
					CurrLine = LinkedListInsert(CurrLine, " movwf SysStringA_H")
				Else
					CurrLine = LinkedListInsert(CurrLine, " movlw low " + SourceTable)
					CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRL")
					CurrLine = LinkedListInsert(CurrLine, " movlw high " + SourceTable)
					CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRH")
				End If
			ElseIf ModeAVR Then
				AddVar "SysReadA", "WORD", 1, 0, "REAL", Origin
				CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA,low(" + SourceTable + "<<1)")
				CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA_H,high(" + SourceTable + "<<1)")
			End If
			RequestSub(CurrSub, "SysReadString")
			If TC = 1 Then CurrLine = LinkedListInsert(CurrLine, " call SysReadString")
			If TC > 1 Then CurrLine = LinkedListInsert(CurrLine, " call SysReadStringPart")
		
		'Anything else, show error
		Case Else:
			Temp = Message("BadValueType")
			Replace Temp, "%value%", Source
			Replace Temp, "%type%", "string"
			Replace Temp, "%var%", DestArrayName
			LogError Temp, Origin
			
		End Select
		
	NEXT
	
	'Set length
	IF TC > 1 Then
		
		CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestArrayName, -1, CurrSub, Origin))
		If ModePIC Then
			If ChipFamily = 16 Then
				CurrLine = LinkedListInsert(CurrLine, " movff SysStringLength, INDF0")
			ElseIf ChipFamily = 15 Then
				CurrLine = LinkedListInsert(CurrLine, " movf SysStringLength,W")
				CurrLine = LinkedListInsert(CurrLine, " movwf INDF0")
			Else
				CurrLine = LinkedListInsert(CurrLine, " movf SysStringLength,W")
				CurrLine = LinkedListInsert(CurrLine, " movwf INDF")
			End If
		ElseIf ModeAVR Then
			CurrLine = LinkedListInsert(CurrLine, " st X, SysStringLength")
		End If
		
	End If
	
	'Enable interrupt
	'CurrLine = LinkedListInsert(CurrLine, "INTON")
	
	Return OutList
End Function

SUB CompileSelect (CompSub As SubType Pointer)
	FoundCount = 0
	Dim As String InLine, Origin, Temp, SelectValue, Condition
	Dim As Integer PD, CC, AL, SCL
	Dim As LinkedListElement Pointer CurrLine, NewCode, FindCase
	
	SCL = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = CurrLine->Value
		IF Left(InLine, 12) = "SELECT CASE " THEN
			
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			FoundCount += 1
			
			SelectValue = Trim(Mid(InLine, 12))
			SCL = 1
			CC = 0
			CurrLine = LinkedListDelete(CurrLine)
			SCT = SCT + 1
			
			FindCase = CurrLine->Next
			Do While FindCase <> 0
				InLine = FindCase->Value
				
				IF Left(InLine, 12) = "SELECT CASE " THEN
					SCL += 1
					
				ElseIf SCL = 1 AND Left(InLine, 5) = "CASE " THEN
					CC += 1
					
					'Trim trailing : from line
					If Right(InLine, 1) = ":" Then
						InLine = Left(InLine, Len(InLine) - 1)
					End If
					
					IF CC = 1 THEN
						FindCase->Value = "SysSelect" + Str(SCT) + "Case" + Str(CC) + LabelEnd
					Else
						If ModePIC Then
							FindCase->Value = " goto SysSelectEnd" + Str(SCT)
							FindCase = LinkedListInsert(FindCase, "SysSelect" + Str(SCT) + "Case" + Str(CC))
						ElseIf ModeAVR Then
							FindCase->Value = " rjmp SysSelectEnd" + Str(SCT)
							FindCase = LinkedListInsert(FindCase, "SysSelect" + Str(SCT) + "Case" + Str(CC) + ":")
						End If
					END IF
					
					Condition = Trim(Mid(InLine, 5))
					IF INSTR(Condition, ";?F") <> 0 THEN
						Origin = Mid(Condition, INSTR(Condition, ";?F"))
						Condition = RTrim(Left(Condition, INSTR(Condition, ";?F") - 1))
					END IF
					
					IF Condition <> "ELSE" THEN
						IF CountOccur(Condition, "';=~<>{}") = 0 THEN Condition = "=" + Condition
						Condition = SelectValue + Condition
						
						NewCode = CompileConditions(Condition, "FALSE", Origin)
						FindCase = LinkedListInsertList(FindCase, NewCode)
						If ModePIC Then
							FindCase = LinkedListInsert(FindCase, " goto SysSelect" + Str(SCT) + "Case" + Str(CC + 1))
						ElseIf ModeAVR Then
							FindCase = LinkedListInsert(FindCase, " rjmp SysSelect" + Str(SCT) + "Case" + Str(CC + 1))
						End If
					END IF
					
				ElseIf Left(InLine, 10) = "END SELECT" THEN
					
					IF SCL = 1 THEN
						FindCase->Value = "SysSelect" + Str(SCT) + "Case" + Str(CC + 1) + LabelEnd
						FindCase = LinkedListInsert(FindCase, "SysSelectEnd" + Str(SCT) + LabelEnd)
					END IF
					SCL = SCL - 1
					
				END IF
				
				'Quit once End Select found
				If SCL = 0 Then Exit Do
				FindCase = FindCase->Next
			Loop
		END IF
		CurrLine = CurrLine->Next
	Loop
	
END SUB

SUB CompileSet (CompSub As SubType Pointer)
	FoundCount = 0
	Dim As String InLine, Origin, Temp
	Dim As String VarName, VarBit, Status
	
	Dim As String Token(20)
	Dim As Integer Tokens
	
	Dim As LinkedListElement Pointer CurrLine
	Dim As PinDirType Pointer CurrPinDir
	
	'Find SET commands
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		InLine = CurrLine->Value
		IF UCase(Left(InLine, 4)) = "SET " THEN
			
			'Get origin
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN 
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			'Get Tokens from line
			GetTokens(InLine, Token(), Tokens)
			'Should be: 1 = SET, 2 = Var, 3 = Bit, 4 = Status
			
			'Delete line from program, will add asm later
			CurrLine = LinkedListDelete(CurrLine)
			
			'Get var/port and bit
			If Tokens = 4 Then
				VarName = Token(2)
				VarBit = Token(3)
				Status = Token(4)
				
				'If writing to a pin, record it
				CurrPinDir = GetPinDirection(VarName + "." + VarBit)
				If CurrPinDir <> 0 Then
					CurrPinDir->WrittenTo = -1
				End If
				
			Else
				'Wrong number of input parameters
				Temp = Message("BadParamsSet")
				LogError Temp, Origin
				GOTO BADSET
			End If
			
			'Generate code
			CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(VarName + "." + VarBit, Status, Origin))
			
			FoundCount = FoundCount + 1
		END IF

BADSET:
		CurrLine = CurrLine->Next
	Loop
END SUB

Function CompileSubCall (InCall As SubCallType Pointer) As LinkedListElement Pointer
	
	Dim As Integer CD, C, F, L, S, AF, StringConstCount, RP, ParamIsFn, LocOfFn, ST
	Dim As String Temp, SendOrigin, ReceiveOrigin, SourceArray, DestArray
	Dim As String ArrayHandler, SourceArrayHandler, TempData, GenName
	Dim As String CallCmd, MacroLineOrigin, SourceFunction
	Dim As LinkedListElement Pointer BeforeCode, AfterCode, BeforePos, AfterPos
	Dim As VariableType Pointer SourcePtr, SourceArrayPtr
	
	Dim As LinkedListElement Pointer OutList, CurrPos, MacroLine
	OutList = LinkedListCreate
	
	'Temporary code lists
	BeforeCode = LinkedListCreate
	AfterCode = LinkedListCreate
	BeforePos = BeforeCode
	AfterPos = AfterCode
	
	StringConstCount = 0
	
	'Load code to call sub into OutList
	With (*InCall)
		
		If .Called->IsMacro Then
			'Dealing with a macro
			'Copy lines over, replacing params where needed
			CurrPos = OutList
			
			MacroLine = .Called->CodeStart->Next
			Do While MacroLine <> 0
				TempData = MacroLine->Value
				'Remove original origin
				MacroLineOrigin = ""
				IF INSTR(TempData, ";?F") <> 0 THEN 
					MacroLineOrigin = " " + Mid(TempData, INSTR(TempData, ";?F"))
					TempData = RTrim(Left(TempData, INSTR(TempData, ";?F") - 1))
					'If macro line has an origin, check if dest line also has one 
					If .Origin <> "" Then
						'If dest line has origin, use that instead of macro line's original
						MacroLineOrigin = " " + Trim(.Origin)
					End If
				END If
				'Replace parameters
				For RP = 1 to .Params
					WholeReplace TempData, .Called->Params(RP).Name, .Param(RP, 1)
				Next
				'Hide macro name (if macro is a function)
				If .Called->IsFunction Then
					Do While InStr(TempData, .Called->Name) <> 0
						Replace (TempData, .Called->Name, Chr(31) + Str((*InCall).CalledID) + Chr(31))
					Loop
				End If
				'Insert line
				If TempData <> "" Then
					'Append insertion origin
					CurrPos = LinkedListInsert(CurrPos, TempData + MacroLineOrigin)
				End If
				MacroLine = MacroLine->Next
			Loop
			Return OutList
			
		Else
			'Not dealing with a macro, so set parameters and call
			'Get call command
			With (*.Called)
				If .IsFunction Then
					'GenName = "FN_" + .Name
					GenName = "FN_" + Chr(31) + Str((*InCall).CalledID) + Chr(31)
				Else
					'GenName = .Name
					GenName = Chr(31) + Str((*InCall).CalledID) + Chr(31)
				End If
				IF .Overloaded Then
					CallCmd = " call " + GenName + Str((*InCall).CalledID)
				Else
					CallCmd = " call " + GenName
				End If
				'Print "Compiling call to " + .Name + " (from " + InCall->Caller->Name + ")"
			End With	
			
			'Optional params
			If (*.Called).ParamCount > .Params Then
				For CD = .Params + 1 To (*.Called).ParamCount
					.Param(CD, 1) = ""
				Next
				.Params = (*.Called).ParamCount
			End If 
			
			'Make code to copy parameters
			FOR CD = 1 TO .Params
				
				'Load defaults into optional parameters
				IF (*.Called).Params(CD).Default <> "" And .Param(CD, 1) = "" Then
					.Param(CD, 1) = (*.Called).Params(CD).Default
					
					'Constants won't have been replaced in default values
					.Param(CD, 1) = ReplaceConstantsLine(.Param(CD, 1), 0)
					.Param(CD, 2) = TypeOfValue(.Param(CD, 1), Subroutine(GetSubID(.Origin)))
				End If
				'Print , .Param(CD, 1), .Param(CD, 2)
				
				'Decide how to pass parameter
				'C = 0: Copy variable
				'C = 1: Copy constant value
				'C = 2: Copy calculation result
				'C = 3: Copy string/array reference
				'C = 4: Copy string into temp array then pass reference
				'C = 5: Concatenate strings, store in temp array, pass reference
				C = 0
				IF IsConst(.Param(CD, 1)) THEN C = 1
				IF IsCalc(.Param(CD, 1)) THEN C = 2
				IF INSTR((*.Called).Params(CD).Type, "$") <> 0 Or INSTR((*.Called).Params(CD).Type, "()") <> 0 Or (*.Called).Params(CD).Type = "STRING" Then
					C = 3
					If InStr(.Param(CD, 1), "+") Then C = 5
				End If
				'IF INSTR(.Param(CD, 1), ";STRING") <> 0 THEN
				'Only search for ;STRING at start, not in middle!
				IF INSTR(.Param(CD, 1), ";STRING") = 1 Then
					Temp = StringStore(Val(MID(.Param(CD, 1), Instr(.Param(CD, 1), ";STRING") + 7))).Value
					If Len(Temp) = 1 And (*.Called).Params(CD).Type <> "STRING" Then
						C = 1
					Else
						C = 4
						If InStr(.Param(CD, 1), "+") Then C = 5
					End If
				End If
				
				'Pass by copying
				If C = 0 OR C = 1 OR C = 2 THEN
					'Calculate new origins
					'Important so that variable types are detected correctly
					SendOrigin = ""
					ReceiveOrigin = ""
					If .Origin <> "" Then
						F = VAL(Mid(.Origin, INSTR(.Origin, "F") + 1))
						L = VAL(Mid(.Origin, INSTR(.Origin, "L") + 1))
						Temp = Mid(.Origin, INSTR(.Origin, "S") + 1)
						IF INSTR(Temp, "D") <> 0 Then Temp = Left(Temp, INSTR(Temp, "D") - 1)
						S = VAL(Temp)
					Else
						'If no origin present, make one up so variable assignments work
						'As long as sub (S) is correct, it will work properly
						'(except for maybe giving misleading error locations)
						F = 0
						L = 0
						S = 0
						For LocOfFn = 0 To SBC
							If Subroutine(LocOfFn) = .Caller Then
								S = LocOfFn
								Exit For
							End If
						Next
					End If
					SendOrigin = ";?F" + Str(F) + "L" + Str(L) + "S" + Str(S) + "D" + Str(.CalledID) + "?"
					ReceiveOrigin = ";?F" + Str(F) + "L" + Str(L) + "S" + Str(.CalledID) + "D" + Str(S) + "?"
					
					If (*.Called.Params(CD).Dir And 1) <> 0 Then
						BeforePos = LinkedListInsert(BeforePos, *.Called.Params(CD).Name + " = " + .Param(CD, 1) + SendOrigin)
					End If
					'Detect parameters that are functions, don't try returning values in them
					ParamIsFn = 0
					LocOfFn = LocationOfSub(.Param(CD, 1), "", .Origin, -1)
					If LocOfFn <> 0 Then
						If Subroutine(LocOfFn)->IsFunction Then ParamIsFn = -1
					End If
					IF C = 0 AND (*.Called.Params(CD).Dir And 2) <> 0 And Not ParamIsFn THEN
						AfterPos = LinkedListInsert(AfterPos, .Param(CD, 1) + " = " + *.Called.Params(CD).Name + ReceiveOrigin)
					ElseIf *.Called.Params(CD).Dir = 2 And C <> 0 Then
						'Error, need to supply output
						Temp = Message("SubParamNotVar")
						Replace Temp, "%value%", .Param(CD, 1)
						Replace Temp, "%param%", *.Called.Name
						LogError Temp, .Origin
					End IF
				END IF
				
				'Pass string using SYSTEMPARRAY
				IF C = 4 Or C = 5 THEN
					'On 16F1, should pass reference to string location if sub will not be changing it
					If C = 4 And ChipFamily = 15 And *.Called.Params(CD).Dir = 1 Then
						'Print "Opportunity for optimisation: "; *.Called.Params(CD).Name; " = "; .Param(CD, 1)
						'Destination array
						DestArray = *.Called.Params(CD).Name
						IF INSTR(DestArray, "()") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "()") - 1)
						IF INSTR(DestArray, "$") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "$") - 1)
						'Create destination array if necessary
						SourcePtr = VarAddress(DestArray, .Called)
						IF SourcePtr = 0 THEN AddVar DestArray, "BYTE", 2, .Called, "POINTER", .Origin
						'Set handler name
						ArrayHandler = "Sys" + DestArray + "Handler"
						AddVar ArrayHandler, "WORD", 1, 0, "REAL", .Origin 'Make handler global
						'Copy reference
						ST = VAL(Mid(.Param(CD, 1), 8))
						StringStore(ST).Used = -1
						BeforePos = LinkedListInsert(BeforePos, " movlw low StringTable" + Str(ST))
						BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler)
						BeforePos = LinkedListInsert(BeforePos, " movlw (high StringTable" + Str(ST) + ") + 128")
						BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler + "_H")
					
					'In other situations, copy string constant into temporary variable, then pass reference to that
					Else
						StringConstCount += 1
						BeforePos = LinkedListInsert(BeforePos, "SYSSTRINGPARAM" + Str(StringConstCount) + "=" + .Param(CD, 1) + .Origin)
						.Param(CD, 1) = "SYSSTRINGPARAM" + Str(StringConstCount)
						.Param(CD, 2) = "STRING"
						AddVar("SYSSTRINGPARAM" + Str(StringConstCount), "STRING", 20, .Caller, "REAL", .Origin)
						C = 3
					End If
				END IF
				
				'Pass array by reference (set pointer)
				IF C = 3 THEN
					'Source
					SourceArray = .Param(CD, 1)
					'IF INSTR(SourceArray, "(") <> 0 THEN SourceArray = Left(SourceArray, INSTR(SourceArray, "(") - 1)
					IF INSTR(SourceArray, "()") <> 0 THEN SourceArray = Left(SourceArray, INSTR(SourceArray, "()") - 1)
					IF INSTR(SourceArray, "$") <> 0 THEN SourceArray = Left(SourceArray, INSTR(SourceArray, "$") - 1)
					
					'Is SourceArray a function?
					LocOfFn = LocationOfSub(.Param(CD, 1), "", .Origin, -1)
					
					'SourceArray is function
					If LocOfFn <> 0 Then
						SourceFunction = SourceArray
						IF INSTR(SourceArray, "(") <> 0 THEN SourceArray = Left(SourceArray, INSTR(SourceArray, "(") - 1)
						SourceArrayPtr = VarAddress(ReplaceFnNames(SourceArray), .Caller)
						
						'Remove function name from SourceArray
						SourceArray = Chr(31) + Str(LocOfFn) + Chr(31)
						
					'SourceArray is not a function
					Else
						SourceFunction = SourceArray
						SourceArrayPtr = VarAddress(ReplaceFnNames(SourceArray), .Caller)
						'If source array not found, show error
						If SourceArrayPtr = 0 Then
							Temp = Message("ArrayNoDec")
							Replace Temp, "%name%", ReplaceFnNames(SourceArray)
							LogError Temp, .Origin
							GoTo CompileNextParam
						End If
					End If
					
					'Destination
					DestArray = *.Called.Params(CD).Name
					'IF INSTR(DestArray, "(") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "(") - 1)
					IF INSTR(DestArray, "()") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "()") - 1)
					IF INSTR(DestArray, "$") <> 0 THEN DestArray = Left(DestArray, INSTR(DestArray, "$") - 1)
					
					'Create destination array if necessary
					SourcePtr = VarAddress(DestArray, .Called)
					IF SourcePtr = 0 THEN AddVar DestArray, "BYTE", 2, .Called, "POINTER", .Origin
					'Print .Param(CD, 1), SourcePtr, SourceArrayPtr
					
					'Set handler name
					ArrayHandler = "Sys" + DestArray + "Handler"
					AddVar ArrayHandler, "WORD", 1, 0, "REAL", .Origin 'Make handler global
					
					'May need to hide function names in array names
					Do While InStr(SourceArray, .Called->Name) <> 0
						Replace (SourceArray, .Called->Name, Chr(31) + Str((*InCall).CalledID) + Chr(31))
					Loop
					Do While InStr(ArrayHandler, .Called->Name) <> 0
						Replace (ArrayHandler, .Called->Name, Chr(31) + Str((*InCall).CalledID) + Chr(31))
					Loop
					SourceArrayHandler = "Sys" + SourceArray + "Handler"
					
					If ModePIC Then
						If SourceArrayPtr->Pointer = "POINTER" Then
							If ChipFamily = 16 Then
								BeforePos = LinkedListInsert(BeforePos, " movff " + SourceArrayHandler + "," + ArrayHandler)
								BeforePos = LinkedListInsert(BeforePos, " movff " + SourceArrayHandler + "_H," + ArrayHandler + "_H")
							Else
								BeforePos = LinkedListInsert(BeforePos, " movf " + SourceArrayHandler + ",W")
								BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler)
								BeforePos = LinkedListInsert(BeforePos, " movf " + SourceArrayHandler + "_H,W")
								BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler + "_H")	
							End If
						Else
							BeforePos = LinkedListInsert(BeforePos, " movlw low " + SourceFunction)
							BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler)
							BeforePos = LinkedListInsert(BeforePos, " movlw high " + SourceArray)
							BeforePos = LinkedListInsert(BeforePos, " movwf " + ArrayHandler + "_H")							
						End If
					ElseIf ModeAVR Then
						If SourceArrayPtr->Pointer = "POINTER" Then
							BeforePos = LinkedListInsert(BeforePos, " lds SysValueCopy," + SourceArrayHandler)
							BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + ",SysValueCopy")
							BeforePos = LinkedListInsert(BeforePos, " lds SysValueCopy," + SourceArrayHandler + "_H")
							BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + "_H,SysValueCopy")
						Else
							BeforePos = LinkedListInsert(BeforePos, " ldi SysValueCopy,low(" + SourceFunction + ")")
							BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + ",SysValueCopy")
							BeforePos = LinkedListInsert(BeforePos, " ldi SysValueCopy,high(" + SourceArray + ")")
							BeforePos = LinkedListInsert(BeforePos, " sts " + ArrayHandler + "_H,SysValueCopy")
						End If
						AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
					End If
				END IF
				
				CompileNextParam:
			Next
			
			'Mark sub as required
			.Called->Required = -1
			'Mark file sub came from as used
			Replace gcINC(.Called->SourceFile, 2), ";", ""
			
		End If
		
	End With
	
	'Copy code to output array
	CurrPos = OutList
	CurrPos = LinkedListInsertList(CurrPos, BeforeCode)
	CurrPos = LinkedListInsert(CurrPos, CallCmd)
	CurrPos = LinkedListInsertList(CurrPos, AfterCode)
	
	Return OutList
End Function

Sub CompileSubCalls(CompSub As SubType Pointer)
	
	Dim As String TempLine, LineFromFile, BeforeFn, FunctionName, FunctionParams, AfterFn
	Dim As String FunctionType, Origin, TempVarName, TempData
	Dim As Integer CD, DS, S, E, BL, FB, F, PD, FoundFunction, MatchScore, BetterMatch
	Dim As Integer L, D, SL, Temp, UseTempVar, FunctionTypeID, CurrSub, FindMatch
	Dim As Integer ParamsInBrackets
	
	Dim As LinkedListElement Pointer CurrLine, NewCallCode, NewCallLine, LineBeforeCall
	
	Dim As SubCallType NewSubCall
	
	Dim TempVarCount(10) As Integer
	
	'Find functions
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		'Check if startup sub is needed for the line because of a constant
		DO WHILE INSTR(UCase(CurrLine->Value), ";STARTUP") <> 0
			Temp = VAL(Mid(CurrLine->Value, INSTR(UCase(CurrLine->Value), ";STARTUP") + 8))
			TempData = ";STARTUP" + Str(Temp)
			Replace CurrLine->Value, TempData, ""
			Replace gcINC(Temp, 2), ";", ""
			RequestSub(0, Trim(gcINC(Temp, 2), Any "; "))
		Loop
		
		For PD = 1 to 10
			TempVarCount(PD) = 0
		Next
		
		FindFunctionsAgain:
		TempLine = CurrLine->Value
		Origin = ""
		IF INSTR(TempLine, ";?F") <> 0 THEN Origin = MID(TempLine, INSTR(TempLine, ";?F"))
		
		'Don't check "ON " statements for functions
		If UCASE(Left(TempLine, 3)) = "ON " Then Goto NextLineFunctions
		
		'Run through list of functions
		FOR CurrSub = 1 To SBC
			
			FunctionName = Subroutine(CurrSub)->Name
			FunctionType = Subroutine(CurrSub)->ReturnType
			FunctionTypeID = CastOrder(FunctionType)
			
			'Check to see if a line contains a function
			If INSTR(UCASE(TempLine), UCase(FunctionName)) = 0 THEN Goto CheckForNextFunction
			
			SearchLineAgain:
			FoundFunction = WholeINSTR(TempLine, FunctionName, 0)
			
			'Avoid calling functions from themselves
			If FoundFunction = 2 And Subroutine(CurrSub)->IsFunction Then
				If CompSub = Subroutine(CurrSub) Then FoundFunction = 0
			END IF
			
			'Have already dealt with line
			If Origin = "" And Left(TempLine, 6) = " call " Then
				FoundFunction = 0
			End If
			
			IF FoundFunction <> 2 And INSTR(UCase(TempLine), UCase(FunctionName)) <> 0 THEN 
				Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
				Goto SearchLineAgain
			END IF
			
			'If it does, call a sub, and get the value after
			IF FoundFunction = 2 THEN
				'Print "Found function: "; FunctionName
				'Check if a temp variable should be used
				UseTempVar = 0
				If Subroutine(CurrSub)->IsFunction Then
					If CountOccur(TempLine, FunctionName, -1) > 1 Then
						UseTempVar = -1
						TempVarCount(FunctionTypeID) += 1
						TempVarName = "SysFn" + FunctionType + Str(TempVarCount(FunctionTypeID))
						AddVar TempVarName, FunctionType, 1, CompSub, "REAL", Origin
					End If
					'Add a variable for the function result
					AddVar Subroutine(CurrSub)->Name, FunctionType, 1, Subroutine(CurrSub), "REAL", Origin
					AddVar Subroutine(CurrSub)->Name, FunctionType, 1, CompSub, "REAL", Origin
				End If
				
				'If line starts with call or GOSUB, remove call
				If Left(TempLine, 5) = "CALL " Then
					TempLine = Trim(Mid(TempLine, 6))
				ElseIf Left(TempLine, 6) = "GOSUB " Then
					TempLine = Trim(Mid(TempLine, 7))
				End If
				
				'Replace the function in the line
				'Get whatever is to the left of the function
				BeforeFn = LEFT(TempLine, INSTR(UCase(TempLine), FunctionName) - 1)
				
				'If there's something before, make sure we actually have a function
				If BeforeFn <> "" And Not Subroutine(CurrSub)->IsFunction Then
					Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
					Goto SearchLineAgain
				End If
				
				'Get the parameters of the function and the text after it
				FunctionParams = ""
				
				AfterFn = MID(TempLine, INSTR(UCase(TempLine), UCase(FunctionName)) + LEN(FunctionName))
				
				'Check to see if parameters are in brackets
				ParamsInBrackets = 0
				For FB = 1 To Len(AfterFn)
					If Mid(AfterFn, FB, 1) = "(" Then
						ParamsInBrackets = -1
						Exit For
					ElseIf Mid(AfterFn, FB, 1) <> " " Then
						Exit For
					End If
				Next
				
				If Subroutine(CurrSub)->IsFunction And (ParamsInBrackets Or Not Subroutine(CurrSub)->Overloaded) Then
					'Function, needs to have parameters in brackets
					BL = 0
					FOR FB = 1 TO LEN(AfterFn)
						IF MID(AfterFn, FB, 1) = "(" THEN BL = BL + 1
						IF MID(AfterFn, FB, 1) = ")" Then
							BL = BL - 1
							IF BL = 0 THEN
								FunctionParams = Trim(Left(AfterFn, FB))
								AfterFn = MID(AfterFn, FB + 1)
								EXIT FOR
							End If
						End If
					Next
				Else
					'Macro/Sub, parameters take up the rest of the line, brackets optional
					FunctionParams = Trim(AfterFn)
					AfterFn = ""
				End If
				
				'Remove origin from FunctionParams
				IF INSTR(FunctionParams, ";?F") <> 0 Then
					FunctionParams = Left(FunctionParams, InStr(FunctionParams, ";?F") - 1)
				End If
				
				If Left(FunctionParams, 1) = "=" Then
					Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
					Goto SearchLineAgain
				End If
				
				'Prepare sub call
				'ExtractParameters(NewSubCall, ReplaceFnNames(FunctionName) + " " + ReplaceFnNames(FunctionParams), Origin)
				ExtractParameters(NewSubCall, FunctionName + " " + FunctionParams, Origin)
				With NewSubCall
					.Called = Subroutine(CurrSub)
					.Caller = CompSub
					.CalledID = CurrSub
					.Origin = Origin
				End With
				
				'Check function being called matches current function best
				If Subroutine(CurrSub)->Overloaded Then
					MatchScore = SubSigMatch(GetSubSig(*Subroutine(CurrSub)), NewSubCall.CallSig)
					BetterMatch = 0
					'Check for better matches
					For FindMatch = CurrSub + 1 To SBC
						If Subroutine(CurrSub)->Name = Subroutine(FindMatch)->Name Then
							If SubSigMatch(GetSubSig(*Subroutine(FindMatch)), NewSubCall.CallSig) > MatchScore Then
								BetterMatch = -1
								Exit For
							End If
						End If
					Next
					
					'Skip this sub if it's not the best
					If BetterMatch Then
						Replace TempLine, FunctionName, CHR(30) + STR(CurrSub) + CHR(30)
						Goto SearchLineAgain
					End If
				End If
				
				'Adjust line
				'Use 31 where name needs removal to prevent trying to handle it twice
				If UseTempVar Then
					TempLine = BeforeFn + TempVarName + AfterFn
				Else
					TempLine = BeforeFn + CHR(31) + Str(CurrSub) + CHR(31) + AfterFn
				End If
				
				'Add destination to origin
				/'
				IF INSTR(TempLine, ";?F") <> 0 THEN
					Dim As String TLeft, TRight, TOrigin, TempStr, TSub
					
					TLeft = LEFT(TempLine, INSTR(TempLine, ";?") + 1)
					TempStr = MID(TempLine, INSTR(TempLine, "?") + 1)
					TOrigin = LEFT(TempStr, INSTR(TempStr, "?") - 1)
					TRight = MID(TempStr, INSTR(TempStr, "?"))
					
					F = VAL(Mid(TOrigin, INSTR(TOrigin, "F") + 1))
					L = VAL(Mid(TOrigin, INSTR(TOrigin, "L") + 1))
					'S = VAL(Mid(TOrigin, INSTR(TOrigin, "S") + 1))
					TSub = Mid(TOrigin, INSTR(TOrigin, "S") + 1)
					If InStr(TSub, "D") <> 0 Then
						TSub = Left(TSub, InStr(TSub, "D") - 1)
					End If
					S = Val(TSub)
					
					'Was D = PD
					D = CurrSub
					'12/4/2008: Should this be used for the new code or not?
					'SL = LocationOfSub(TempLine)
					'IF SL <> 0 THEN S = SL
					
					TempLine = TLeft + "F" + STR(F) + "L" + STR(L) + "S" + STR(D) + "D" + STR(S) + TRight
				END IF
				'/
				
				'Write back code
				'Print DS, BeforeFn, FunctionName, FunctionParams, AfterFn
				If Subroutine(CurrSub)->IsFunction Then
					CurrLine->Value = ";FNSTART," + CHR(31) + Str(CurrSub) + CHR(31)
				Else
					CurrLine = LinkedListDelete(CurrLine)
				End If
				LineBeforeCall = CurrLine
				CurrLine = LinkedListInsertList(CurrLine, CompileSubCall(@NewSubCall))
				
				'Record call
				If Not NewSubCall.Called->IsMacro Then
					RecordSubCall(CompSub, NewSubCall.Called)
				End If
				
				If UseTempVar Then
					CurrLine = LinkedListInsert(CurrLine, TempVarName + "=" + Chr(31) + Str(CurrSub) + CHR(31))
					LinkedListInsert(CurrLine, TempLine)
				Else
					LinkedListInsert(CurrLine, TempLine)
				End If
				
				'Code commented out 13/4/2009
				'Need to check line again, in case of nested functions
				'If Subroutine(CurrSub)->IsMacro Then
					CurrLine = LineBeforeCall
					GoTo NextLineFunctions
				'End If
				''Continue searching line
				'GOTO FindFunctionsAgain
				
			END If
			
			CheckForNextFunction:
			'Use 30 for temporarily removing name, ie when close match occurs
			Dim OldName As String
			Do While Instr(TempLine, Chr(30)) <> 0
				OldName = MID(TempLine, Instr(TempLine, Chr(30)) + 1)
				OldName = Left(OldName, INSTR(OldName, Chr(30)) - 1)
				Replace TempLine, Chr(30) + OldName + Chr(30), Subroutine(Val(OldName))->Name
			Loop
			
		NEXT
		
		NextLineFunctions:
		CurrLine = CurrLine->Next
	Loop
	
	'Deal with any missed names
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		'Delete any lines with just a sub name
		If Left(CurrLine->Value, 1) = Chr(31) And InStr(CurrLine->Value, "=") = 0 Then
			CurrLine = LinkedListDelete(CurrLine)
		Else
			'Replace tokens with names
			CurrLine->Value = ReplaceFnNames(CurrLine->Value)
		End If
		CurrLine = CurrLine->Next
	Loop
	
End Sub

Sub CompileTables
	Dim As String Temp, Source, Table, ThisItem
	Dim As Integer PD, SP, CurrTableByte, StringTablesUsed
	
	Dim As LinkedListElement Pointer CurrLine
	Dim As SubType Pointer TableSub
	
	'Add string lookup tables
	StringTablesUsed = 0
	For PD = 1 To SSC
		If StringStore(PD).Used Then
			StringTablesUsed = -1
			Exit For
		End If
	Next
	
	IF StringTablesUsed THEN
 		IF VBS = 1 THEN PRINT : PRINT SPC(5); Message("StringTable")
 		
 		'Create sub for string tables
 		TableSub = NewSubroutine("SysStringTables")
 		SBC += 1: Subroutine(SBC) = TableSub
 		CurrLine = TableSub->CodeStart
 		TableSub->Compiled = -1
 		TableSub->Required = 0
 		TableSub->NoReturn = -1
 		
 		AddVar "StringPointer", "BYTE", 1, Subroutine(0), "REAL", ""
 		AddVar "SysStringA", "WORD", 1, Subroutine(0), "REAL", ""
		
 		If ModePIC And ChipFamily <> 16 Then
 			If ChipFamily = 12 Then
				CurrLine = LinkedListInsert(CurrLine, " movf SysStringA,W")
				CurrLine = LinkedListInsert(CurrLine, " incf SysStringA,F")
				CurrLine = LinkedListInsert(CurrLine, " movwf PCL")
				LogWarning Message("WarningUnsafeString")
			Else
				'Get current pointer location
				CurrLine = LinkedListInsert(CurrLine, " movf SysStringA_H,W")
				CurrLine = LinkedListInsert(CurrLine, " movwf PCLATH")
				CurrLine = LinkedListInsert(CurrLine, " movf SysStringA,W")
				'Code will increment pointer for next access
				CurrLine = LinkedListInsert(CurrLine, " incf SysStringA,F")
				CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS,Z")
				CurrLine = LinkedListInsert(CurrLine, " incf SysStringA_H,F")
				'Alter PCL
				CurrLine = LinkedListInsert(CurrLine, " movwf PCL")
			End If
			
		End If
		
		For PD = 1 TO SSC
 			Table = "StringTable" + Str(PD) + LabelEnd
 			Source = StringStore(PD).Value
 			If StringStore(PD).Used Then
	 			CurrLine = LinkedListInsert(CurrLine, "")
	 			CurrLine = LinkedListInsert(CurrLine, Table)
	 			If ModePIC Then
	 				'Table on low/mid range chip, use retlw
	 				If ChipFamily <> 16 THEN
	  					CurrLine = LinkedListInsert(CurrLine, " retlw " + Str(LEN(Source)))
						For SP = 1 TO LEN(Source)
							Temp = " retlw " + Str(ASC(Mid(Source, SP, 1)))
							IF Mid(Source, SP, 1) <> "=" THEN Temp = Temp + Chr(9) + ";" + Mid(Source, SP, 1)
							IF Mid(Source, SP, 1) = "=" THEN Temp = Temp + Chr(9) + "; (equals)"
							CurrLine = LinkedListInsert(CurrLine, Temp)
						Next
	   			'Table on high end, can use dw and then read with tblrd
	 				Else
						PCC += 1
						PreserveCode(PCC) = Chr(8) + " dw " + STR(LEN(Source) * 256 + LEN(Source)) + "," + CHR(34) + Source + CHR(34)
						CurrLine = LinkedListInsert(CurrLine, "PRESERVE " + Str(PCC))
	 				End If
	 			
	 			ElseIf ModeAVR Then
	 				Temp = Str(LEN(Source))
	 				For SP = 1 TO LEN(Source)
	 					Temp = Temp + "," + Str(ASC(Mid(Source, SP, 1)))
	 				Next
	 				'Need to pad with 0 to make even length
	 				If Len(Source) Mod 2 = 0 Then
	 					Temp = Temp + ",0"
	 				End If
	 				CurrLine = LinkedListInsert(CurrLine, ".DB " + Temp)
	 			End If
	 			CurrLine = LinkedListInsert(CurrLine, "")
	 			
	 			TableSub->Required = -1
 			End If
		Next
	End If
	
	'Add data tables (program memory)
	IF DataTables > 0 Then
		If VBS = 1 THEN PRINT : PRINT SPC(5); Message("DataTable")
		For PD = 1 TO DataTables
			
			'If no data memory, move all tables to program memory
			If DataTable(PD).Used And DataTable(PD).StoreLoc = 1 And ChipEEPROM = 0 Then
				LogWarning Message("WarningNoEEPROMTable")
				DataTable(PD).StoreLoc = 0
			End If
			
			If DataTable(PD).Used And DataTable(PD).StoreLoc = 0 THEN
				If VBS = 1 Then Print Spc(10); DataTable(PD).Name
				DataTable(PD).Item(0) = DataTable(PD).Items
  				
  				'Create sub for data tables
  				'Need to create a table for each byte in data
		 		For CurrTableByte = 0 To GetTypeSize(DataTable(PD).Type) - 1
			 		Table = GetByte("Table" + DataTable(PD).Name, CurrTableByte)
			 		TableSub = NewSubroutine(GetByte(DataTable(PD).Name, CurrTableByte))
			 		
			 		SBC += 1: Subroutine(SBC) = TableSub
			 		CurrLine = TableSub->CodeStart
			 		TableSub->Compiled = -1
			 		TableSub->Required = -1
	  				TableSub->NoReturn = -1
	  				
					If ModePIC Then
		   				AddVar "DataPointer", "BYTE", 1, Subroutine(0), "REAL", ""
		   				IF ChipFamily = 12 THEN
								CurrLine = LinkedListInsert(CurrLine, " movf DataPointer, W")
								CurrLine = LinkedListInsert(CurrLine, " addwf PCL, F")
		   				ElseIf ChipFamily = 14 Or ChipFamily = 15 Then
								CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(DataTable(PD).Items + 1))
								CurrLine = LinkedListInsert(CurrLine, " subwf DataPointer, W")
								CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS, C")
								CurrLine = LinkedListInsert(CurrLine, " retlw 0")
								CurrLine = LinkedListInsert(CurrLine, " movf DataPointer, W")
								CurrLine = LinkedListInsert(CurrLine, " addlw 1")
								CurrLine = LinkedListInsert(CurrLine, " bcf STATUS, C")
								CurrLine = LinkedListInsert(CurrLine, " addlw low " + Table)
								CurrLine = LinkedListInsert(CurrLine, " movwf DataPointer")
								CurrLine = LinkedListInsert(CurrLine, " movlw high " + Table)
								CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS, C")
								CurrLine = LinkedListInsert(CurrLine, " addlw 1")
								CurrLine = LinkedListInsert(CurrLine, " movwf PCLATH")
								CurrLine = LinkedListInsert(CurrLine, " movf DataPointer, W")
								CurrLine = LinkedListInsert(CurrLine, Table)
								CurrLine = LinkedListInsert(CurrLine, " movwf PCL")
		   				ElseIf ChipFamily = 16 Then
								CurrLine = LinkedListInsert(CurrLine, " movlw " + Str(DataTable(PD).Items + 1))
								CurrLine = LinkedListInsert(CurrLine, " cpfslt DataPointer")
								CurrLine = LinkedListInsert(CurrLine, " retlw 0")
								
								CurrLine = LinkedListInsert(CurrLine, " movf DataPointer, W")
								CurrLine = LinkedListInsert(CurrLine, " bcf STATUS, C")
								CurrLine = LinkedListInsert(CurrLine, " addlw low " + Table)
								CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRL")
								CurrLine = LinkedListInsert(CurrLine, " movlw high " + Table)
								CurrLine = LinkedListInsert(CurrLine, " movwf TBLPTRH")
								CurrLine = LinkedListInsert(CurrLine, " btfsc STATUS, C")
								CurrLine = LinkedListInsert(CurrLine, " incf TBLPTRH, F")
								CurrLine = LinkedListInsert(CurrLine, " tblrd*")
								CurrLine = LinkedListInsert(CurrLine, " movf TABLAT, W")
								CurrLine = LinkedListInsert(CurrLine, " return")
								CurrLine = LinkedListInsert(CurrLine, Table)
								Temp = ""
								FOR SP = 0 TO DataTable(PD).Items
									ThisItem = GetByte(Str(DataTable(PD).Item(SP)), CurrTableByte)
									If Len(Temp) + Len(ThisItem) >= 79 And Temp <> "" And (SP Mod 2) = 0 Then
										CurrLine = LinkedListInsert(CurrLine, " db " + Temp)
										Temp = ThisItem
									Else
										If Temp = "" Then
											Temp = ThisItem
										Else
											Temp = Temp + "," + ThisItem
										End If
									End If
								NEXT
								If Temp <> "" Then CurrLine = LinkedListInsert(CurrLine, " db " + Temp)
		   				END IF
		   				If ChipFamily <> 16 Then
								FOR SP = 0 TO DataTable(PD).Items
				 					CurrLine = LinkedListInsert(CurrLine, " retlw " + GetByte(Str(DataTable(PD).Item(SP)), CurrTableByte))
								NEXT
		   				END If
		   				
					ElseIf ModeAVR Then
						'4/1/2010: added << 2 to ldi
						CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA, low(" + Table + "<<1)")
						CurrLine = LinkedListInsert(CurrLine, " ldi SysReadA_H, high(" + Table + "<<1)")
						CurrLine = LinkedListInsert(CurrLine, " add SysReadA,SysValueCopy")
						CurrLine = LinkedListInsert(CurrLine, " brcc PC + 2")
						CurrLine = LinkedListInsert(CurrLine, " inc SysReadA_H")
						CurrLine = LinkedListInsert(CurrLine, " lpm") 'Value will be read, stored in R0
						CurrLine = LinkedListInsert(CurrLine, " ret")
						
						Temp = ""
						FOR SP = 0 TO DataTable(PD).Items
		 					If Temp = "" Then
			 					Temp = GetByte(Str(DataTable(PD).Item(SP)), CurrTableByte)
		 					Else
		 						Temp = Temp + "," + GetByte(Str(DataTable(PD).Item(SP)), CurrTableByte)
		 					End If
		 					
						NEXT
						CurrLine = LinkedListInsert(CurrLine, Table + ":")
						CurrLine = LinkedListInsert(CurrLine, " .DB " + Temp)
						AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
						AddVar "SysReadA", "WORD", 1, 0, "REAL", ""
					End If
	  				
		 		Next
			END If
 		NEXT
	End IF

END SUB

Function CompileVarSet (SourceIn As String, Dest As String, Origin As String) As LinkedListElement Pointer
	
	Dim As String SType, DType, Temp, DestTemp, SourceTemp, CSource
	Dim As String LTemp, HTemp, UTemp, ETemp, STemp, Source, ReferencedSub
	Dim As Integer CurrentSub, DestSub, CurrVarByte, LastConst, ThisConst
	Dim As Integer DestReg, DestIO, SourceReg, SourceIO, L, H, U, E, CD
	Dim As LongInt S
	Dim As PinDirType Pointer CurrPinDir
	
	Dim As LinkedListElement Pointer CurrLine, OutList
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Initialise
	Source = SourceIn
	CurrentSub = GetSubID(Origin)
	IF INSTR(Origin, "D") <> 0 Then DestSub = GetDestSub(Origin) Else DestSub = CurrentSub 
	
	'Get types
	If Instr(Source, "[") <> 0 And InStr(Source, "]") <> 0 Then
		'Remove cast from source name
		'Show warning if it's not the first thing
		If Left(Source, 1) <> "[" Then
			LogWarning(Message("WarningCastBadPlace"), Origin)
		End If
		SType = Mid(Source, InStr(Source, "[") + 1)
		Source = Left(Source, InStr(Source, "[") - 1)
		Source = Source + Mid(SType, InStr(SType, "]") + 1)
		SType = UCase(Left(SType, InStr(SType, "]") - 1))
		If IsConst(Source) Then SType = "CONST"
		
	Else
		SType = TypeOfVar(Source, Subroutine(CurrentSub))
		If IsConst(Source) Then SType = "CONST"
		IF INSTR(Source, ";STRING") <> 0 THEN
			If Len(GetString(Source)) = 1 Then
				SType = "CONST"
				Source = Str(ASC(GetString(Source)))
			Else
				SType = "SCONST"
			End If
		End If
	End If
	
	If Instr(Dest, "[") <> 0 And InStr(Dest, "]") <> 0 Then
		'Remove cast from source name
		'Show warning if it's not the first thing
		If Left(Dest, 1) <> "[" Then
			LogWarning(Message("WarningCastBadPlace"), Origin)
		End If
		DType = Mid(Dest, InStr(Dest, "[") + 1)
		Dest = Left(Dest, InStr(Dest, "[") - 1)
		Dest = Dest + Mid(DType, InStr(DType, "]") + 1)
		DType = UCase(Left(DType, InStr(DType, "]") - 1))
		If IsConst(Dest) Then SType = "CONST"
		
	Else
		DType = TypeOfVar(Dest, Subroutine(DestSub))
	End If
	
	'Don't copy var to itself
	If Source = Dest Then
		Return OutList
	End If
	
	DestReg = IsRegister(Dest)
	DestIO = IsIOReg(Dest)
	SourceReg = IsRegister(Source)
	SourceIO = IsIOReg(Source)
	
	'Record reads and writes (for auto pin direction setting)
	If DType = "BIT" Or DType = "BYTE" Then
		'If writing to a pin/port, record it
		'Unless setting port to 0 - this is done by InitSys and should not be recorded
		CurrPinDir = GetPinDirection(Dest)
		If CurrPinDir <> 0 Then
			If Not IsConst(Source) Or MakeDec(Source) <> 0 Or DType = "BIT" Then
				CurrPinDir->WrittenTo = -1
			End If
		End If
	End If
	If SType = "BIT" Or SType = "BYTE" Then
		'If reading from a pin/port, record it
		CurrPinDir = GetPinDirection(Source)
		If CurrPinDir <> 0 Then
			CurrPinDir->ReadFrom = -1
		End If
	End If
	
	'Record references to subroutines (for indirect calling)
	If Left(Source, 1) = "@" Then
		'Print "Reference found:" + Source
		ReferencedSub = Mid(Source, 2)
		RequestSub(0, ReferencedSub, "")
	End If
	
	'Generate copy code
	'Copies between integers of same or different size
	If IsIntType(SType) And IsIntType(DType) Then
		'Print "New: "; Dest; " = "; Source
		'Don't add variables here, will result in duplicate definitions
		'(if setting VAR_H here, then VAR is defined as word later)
		'If DType = "BYTE" Then AddVar Dest, "BYTE", 1, 0, "REAL", Origin
		'If SType = "BYTE" Then AddVar Source, "BYTE", 1, 0, "REAL", Origin
		
		'Need to check if big constant is getting put into a small variable
		If SType = "CONST" And Not AllowOverflow Then
			If InStr(Source, "@") = 0 Then
				S = MakeDec(Source)
				If Not IsValidValue(S, DType) Then
					Temp = Message("BadValueType")
					Replace Temp, "%value%", Source
					Replace Temp, "%type%", LCase(DType)
					Replace Temp, "%var%", Dest
					LogError Temp, Origin
					Source = "0"
				End If
			End If
		End If
		
		'Remember last value loaded in to W/SysValueCopy
		'-1 = nothing loaded, or unknown
		LastConst = -1
		
		For CurrVarByte = 0 To GetTypeSize(DType) - 1
			
			'Copy each byte
			'Source is a constant, or source is too small
			If SType = "CONST" Or GetTypeSize(SType) <= CurrVarByte Then
				'For integer:
				'MakeTwosComp
				'Get value to load in to current byte
				If SType = "CONST" Then
					If Instr(Source, "@") <> 0 Then
						S = 1
						'Is "high" or "low" already present?
						If Left(Source, 1) <> "@" Then
							STemp = Source
							Replace STemp, "@", ""
						Else
							STemp = GetByte(Source, CurrVarByte)
						End If
						ThisConst = -1
					Else
						STemp = GetByte(Source, CurrVarByte)
						S = Val(STemp)
						ThisConst = S
					End If
				Else
					'Trying to load high byte of smaller variable
					S = 0
					STemp = "0"
					ThisConst = 0
				End If
				
				'Set byte
				If ModePIC Then
					If ChipFamily = 16 And S = 255 Then
						CurrLine = LinkedListInsert(CurrLine, " setf " + GetByte(Dest, CurrVarByte))
					ElseIf S > 0 Then
						If LastConst = -1 Or LastConst <> ThisConst Then
							CurrLine = LinkedListInsert(CurrLine, " movlw " + STemp)
							LastConst = ThisConst
						End If
						CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(Dest, CurrVarByte))
					Else
						CurrLine = LinkedListInsert(CurrLine, " clrf " + GetByte(Dest, CurrVarByte))
					End If
				ElseIf ModeAVR Then
					If DestReg Then
						AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
						CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy," + STemp)
						CurrLine = LinkedListInsert(CurrLine, " mov " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
					Else
						'Note: If writing to a register, ldi then mov must be used
						'This will be optimised to ldi only later
						'If no ldi is present, will have just a mov later
						If LastConst = -1 Or LastConst <> ThisConst Then
							CurrLine = LinkedListInsert(CurrLine, " ldi SysValueCopy," + STemp)
							AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
							LastConst = ThisConst
						End If
						If DestIO Then
							CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
						Else
							CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
						End If
					End If
				End If
				
			'Source is a variable
			Else
				'Copy byte from variable
				If ModePIC Then
					If ChipFamily = 16 Then
						CurrLine = LinkedListInsert(CurrLine, " movff " + GetByte(Source, CurrVarByte) + "," + GetByte(Dest, CurrVarByte))
					Else
						CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(Source, CurrVarByte) + ",W")
						CurrLine = LinkedListInsert(CurrLine, " movwf " + GetByte(Dest, CurrVarByte))
					End If
				ElseIf ModeAVR Then
					If DestReg Then
						If SourceReg Then
							CurrLine = LinkedListInsert(CurrLine, " mov " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
						ElseIf SourceIO Then
							CurrLine = LinkedListInsert(CurrLine, " in " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
						Else
							CurrLine = LinkedListInsert(CurrLine, " lds " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
						End If
					ElseIf DestIO Then
						If SourceReg Then
							CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
						ElseIf SourceIO Then
							CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy," + GetByte(Source, CurrVarByte))
							CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
							AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
						Else
							CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + GetByte(Source, CurrVarByte))
							CurrLine = LinkedListInsert(CurrLine, " out " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
							AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
						End If
					Else
						If SourceReg Then
							CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + "," + GetByte(Source, CurrVarByte))
						ElseIf SourceIO Then
							CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy," + GetByte(Source, CurrVarByte))
							CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
							AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
						Else
							CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + GetByte(Source, CurrVarByte))
							CurrLine = LinkedListInsert(CurrLine, " sts " + GetByte(Dest, CurrVarByte) + ",SysValueCopy")
							AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
						End If
					End If
				End If
				
			End If
			
		Next
		
		'Variable copied, all done!
		Return OutList
	End If
	
	'Copies to or from non-integer types
	'Print "Old: "; Dest; " = "; Source
	Select Case DType
	'Copy to bit
	Case "BIT":
		DestTemp = FixBit(Dest, Origin): If INSTR(DestTemp, ".") <> 0 Then Replace DestTemp, ".", ","
		
		'Add var that contains bit
		IF INSTR(DestTemp, ",") Then
			Temp = Left(DestTemp, INSTR(DestTemp, ",") - 1)
			AddVar Temp, "BYTE", 1, 0, "REAL", Origin
		End If
		
		Select Case SType
		'bit > bit
		Case "BIT":
			
			If ModePIC Then
				SourceTemp = FixBit(Source, Origin): If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","
				
				CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
				CurrLine = LinkedListInsert(CurrLine, " btfsc " + SourceTemp)
				CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
				
			ElseIf ModeAVR Then
				
				'Print "Bit set " + DestTemp + " to value of " + SourceTemp
				
				'Split dest into var and bit, get source
				Dim As String DestVarName, DestVarBit
				SourceTemp = FixBit(Source, Origin)
				If InStr(DestTemp, ",") <> 0 Then
					DestVarName = Trim(Left(DestTemp, InStr(DestTemp, ",") - 1))
					DestVarBit = Trim(Mid(DestTemp, InStr(DestTemp, ",") + 1))
				Else
					Print "Internal error in CompileVarSet bit > bit: " + DestTemp
				End If
				
				If IsRegister(DestVarName) Then
					CurrLine = LinkedListInsert(CurrLine, " cbr " + DestVarName + ",1<<" + DestVarBit)
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " sbr " + DestVarName + ",1<<" + DestVarBit)
					
				ElseIf IsLowIOReg(DestVarName) Then
					CurrLine = LinkedListInsert(CurrLine, " cbi " + DestVarName + "," + DestVarBit)
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " sbi " + DestVarName + "," + DestVarBit)
					
				Else
					CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + DestVarName)
					
					CurrLine = LinkedListInsert(CurrLine, " cbr SysValueCopy,1<<" + DestVarBit)
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(SourceTemp + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " sbr SysValueCopy,1<<" + DestVarBit)
					
					CurrLine = LinkedListInsert(CurrLine, " sts " + DestVarName + ",SysValueCopy")
					If Not IsIOReg(DestVarName) Then AddVar DestVarName, "BYTE", 1, 0, "REAL", Origin
					AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
				End If
				
			End If
		
		'byte > bit / word > bit / integer > bit
			Case "BYTE", "WORD", "INTEGER", "LONG":
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
				CurrLine = LinkedListInsert(CurrLine, " movf " + Source + ",F")
				CurrLine = LinkedListInsert(CurrLine, " btfss STATUS,Z")
				CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
			ElseIf ModeAVR Then
				
			End If
		
		'single > bit
		Case "SINGLE":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'string > bit
		Case "STRING":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'const > bit
		Case "CONST":
			S = MakeDec(Source)
			If ModePIC Then
				If S = 0 Then
					CurrLine = LinkedListInsert(CurrLine, " bcf " + DestTemp)
				Else
					CurrLine = LinkedListInsert(CurrLine, " bsf " + DestTemp)
				End If
			ElseIf ModeAVR Then
				If S = 0 Then
					CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(FixBit(Dest, Origin), "0", Origin))
				Else
					CurrLine = LinkedListInsertList(CurrLine, GenerateBitSet(FixBit(Dest, Origin), "1", Origin))
				End If
			End If
		
		'sconst > bit
		Case "SCONST":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
			
		End Select
		
	'Copy to byte
	Case "BYTE":
		AddVar Dest, "BYTE", 1, 0, "REAL", Origin
		Select Case SType
		'bit > byte
		Case "BIT"
			SourceTemp = FixBit(Source, Origin): If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","
			
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest)
				CurrLine = LinkedListInsert(CurrLine, " btfsc " + SourceTemp)
				CurrLine = LinkedListInsert(CurrLine, " incf " + Dest + ",F")
			ElseIf ModeAVR Then
				If IsRegister(Dest) Then
					CurrLine = LinkedListInsert(CurrLine, " clr " + Dest)
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " inc " + Dest)
				Else
					CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
					CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysValueCopy", Dest, Origin))
					AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
				End If
			End If
		
		'single > byte
		Case "SINGLE":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'string > byte
		Case "STRING":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
			
		'sconst > byte
		Case "SCONST":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
			
		End Select
		
	'Copy to word or integer
	Case "WORD", "INTEGER":
		Select Case SType
		'bit > word
		Case "BIT":
			SourceTemp = FixBit(Source, Origin): If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","
			
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest)
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_H")
				CurrLine = LinkedListInsert(CurrLine, " btfsc " + SourceTemp)
				CurrLine = LinkedListInsert(CurrLine, " incf " + Dest + ",F")
			ElseIf ModeAVR Then
				If IsRegister(Dest) Then
					CurrLine = LinkedListInsert(CurrLine, " clr " + Dest)
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " inc " + Dest)
					CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_H")
				Else
					CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
					CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysValueCopy", Dest, Origin))
					AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
				End If
			End If
			
		'single > word
			Case "SINGLE":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'string > word
		Case "STRING":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'sconst > word
		Case "SCONST":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
			
		End Select
		
	'Copy to long
	Case "LONG":
		Select Case SType
		'bit > long
		Case "BIT":
			SourceTemp = FixBit(Source, Origin): If INSTR(SourceTemp, ".") <> 0 Then Replace SourceTemp, ".", ","
			
			If ModePIC Then
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest)
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_H")
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_U")
				CurrLine = LinkedListInsert(CurrLine, " clrf " + Dest + "_E")
				CurrLine = LinkedListInsert(CurrLine, " btfsc " + SourceTemp)
				CurrLine = LinkedListInsert(CurrLine, " incf " + Dest + ",F")
			ElseIf ModeAVR Then
				If IsRegister(Dest) Then
					CurrLine = LinkedListInsert(CurrLine, " clr " + Dest)
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " inc " + Dest)
					CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_H")
					CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_U")
					CurrLine = LinkedListInsert(CurrLine, " clr " + Dest + "_E")
				Else
					CurrLine = LinkedListInsert(CurrLine, " clr SysValueCopy")
					CurrLine = LinkedListInsertList(CurrLine, CompileConditions(Source + "=1", "TRUE", Origin))
					CurrLine = LinkedListInsert(CurrLine, " inc SysValueCopy")
					CurrLine = LinkedListInsertList(CurrLine, CompileVarSet("SysValueCopy", Dest, Origin))
					AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
				End If
			End If
		
		'single > long
		Case "SINGLE":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'string > long
		Case "STRING":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'sconst > long
		Case "SCONST":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
			
		End Select
			
	'Copy to single
	Case "SINGLE":
		Select Case SType
		'bit > single
		Case "BIT":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'byte > single
		Case "BYTE":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'word > single
		Case "WORD":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'integer > single
		Case "INTEGER":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'single > single
		Case "SINGLE":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'string > single
		Case "STRING":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'const > single
		Case "CONST":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
		
		'sconst > single
		Case "SCONST":
			If ModePIC Then
				
			ElseIf ModeAVR Then
				
			End If
			
		End Select
		
	'Copy to string
	Case "STRING":
		OutList = CompileString(Dest + "=" + SourceIn, Origin)
		
	End Select
	
	Return OutList
End Function

SUB CompileVars (CompSub As SubType Pointer)
	Dim As String InLine, Origin, Temp, DestVar, SourceData
	Dim As String VarType, SourceOld, ErrorTemp
	Dim As Integer CD, PD, T, DisableInt, CTR, AIC, SourceSub, DestSub
	Dim As PinDirType Pointer CurrPinDir
	
	Dim As LinkedListElement Pointer CurrLine, NewCode, FindLine
	Dim As CodeSection Pointer NewSection
	
	CurrLine = CompSub->CodeStart->Next
	FoundCount = 0
	Do While CurrLine <> 0
		InLine = CurrLine->Value
		
		If INSTR(InLine, "=") <> 0 THEN
			'Don't try to do anything if = follows "dw" or "retlw"
			IF INSTR(LCase(InLine), " dw ") <> 0 THEN GOTO CompNextVar
			IF INSTR(LCase(InLine), " retlw ") <> 0 THEN GOTO CompNextVar
			
			'Allow overflow?
			AllowOverflow = 0
			If INSTR(LCase(InLine), "[ao]") <> 0 Then
				Replace InLine, "[ao]", ""
				AllowOverflow = 1
			End If
			
			'Get origin and dest sub
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
				CurrLine->Value = InLine
			END IF
			SourceSub = GetSubID(Origin)
			IF INSTR(Origin, "D") = 0 Then
				DestSub = SourceSub
			Else
				DestSub = GetDestSub(Origin)
			End If
			
			'Remove ALL spaces
			DO WHILE INSTR(InLine, " ") <> 0: Replace InLine, " ", "": LOOP
			
			'Get source and destination
			DestVar = Left(InLine, INSTR(InLine, "=") - 1)
			SourceData = Mid(InLine, INSTR(InLine, "=") + 1)
			
			'Ensure that a variable is being set
			Temp = DestVar
			IF INSTR(Temp, ".") <> 0 THEN Temp = Left(Temp, INSTR(Temp, ".") - 1)
			If IsConst(Temp) THEN
				ErrorTemp = Message("NotAVariable")
				Replace ErrorTemp, "%value%", Temp
				LogError ErrorTemp, Origin
				GoTo CompNextVar
			END IF
			
			'Load an array with a list of values?
			If INSTR(SourceData, ",") <> 0 And IsArray(DestVar, Subroutine(DestSub)) Then
				CurrLine = LinkedListDelete(CurrLine)
				CurrLine = LinkedListInsertList(CurrLine, CompileWholeArray(InLine, Origin))
				GOTO CompNextVar
			END IF
 
			'Check if command involves setting a string
			'IF (IsString(DestVar, Subroutine(DestSub)) Or INSTR(DestVar, "()") <> 0) And (IsString(SourceData, Subroutine(SourceSub))) Or (INSTR(SourceData, "()") <> 0) THEN
			IF IsString(DestVar, Subroutine(DestSub)) Or INSTR(DestVar, "()") <> 0 Then
				CurrLine = LinkedListDelete(CurrLine)
				CurrLine = LinkedListInsertList(CurrLine, CompileString(InLine, Origin))
				GOTO CompNextVar
			END IF
			
			'Call another sub to generate code
			If IsCalc(SourceData) Then
				
				'Add var
				AddVar DestVar, "BYTE", 1, Subroutine(DestSub), "REAL", Origin
				VarType = TypeOfVar(DestVar, Subroutine(DestSub))
				
				'Compile calculation
				FreeCalcVar ""
				SourceOld = SourceData
				NewSection = 0
				CompileCalc SourceData, DestVar, Origin, NewSection
				
				'Finish Calculation
				'If no code has been produced, set var to final value
				IF NewSection->CodeList = 0 THEN
					NewCode = CompileVarSet(SourceData, DestVar, Origin)
				Else
					NewCode = NewSection->CodeList
					
					CurrPinDir = GetPinDirection(DestVar)
					If CurrPinDir <> 0 Then
						CurrPinDir->WrittenTo = -1
					End If
				END IF
  
				If ModePIC Then
					'Get last line
					FindLine = NewCode->Next
					If FindLine <> 0 Then
						Do While FindLine->Next <> 0
							FindLine = FindLine->Next
						Loop
						'Change destination of last operation
						Temp = FindLine->Prev->Value
						IF WholeINSTR(Temp, DestVar) = 2 AND INSTR(Temp, "wf") <> 0 And INSTR(Temp, " movwf ") = 0 THEN
							FindLine = LinkedListDelete(FindLine)
							Replace FindLine->Value, ",W", ",F"
						END If
					End If
				End If
				
				'Re-add asm calculations
				FindLine = NewCode
				Do While FindLine <> 0
					Temp = UCase(FindLine->Value)
					IF INSTR(Temp, "SYSASMCALC") <> 0 Then
						T = VAL(Mid(Temp, INSTR(Temp, "SYSASMCALC") + 10))
						Replace FindLine->Value, "SYSASMCALC" + Str(T), CalcAtAsm(T)
					End If
					FindLine = FindLine->Next
				Loop
				
				'Write the assembly code
				If NewCode->Next <> 0 Then
					CurrLine = LinkedListDelete(CurrLine)
					CurrLine = LinkedListInsertList(CurrLine, NewCode)
				Else
					CurrLine = LinkedListDelete(CurrLine)
				End If
				
			Else
				'Not a calculation, compile the variable setting operation
				'Trim brackets from SourceData
				If Left(SourceData, 1) = "(" And Right(SourceData, 1) = ")" And CountOccur(SourceData, "';()") = 2 Then
					SourceData = Mid(SourceData, 2, Len(SourceData) - 2)
				End If
				
				'Add variables
				AddVar SourceData, "BYTE", 1, Subroutine(SourceSub), "REAL", Origin
				AddVar DestVar, "BYTE", 1, Subroutine(DestSub), "REAL", Origin
				
				'Get code
				NewCode = CompileVarSet(SourceData, DestVar, Origin)
				
				'Add code
				If NewCode->Next <> 0 Then
					CurrLine = LinkedListDelete(CurrLine)
					CurrLine = LinkedListInsertList(CurrLine, NewCode) 
				Else
					CurrLine = LinkedListDelete(CurrLine)
				End If
				
			End If
  
			FoundCount = FoundCount + 1
		END IF

CompNextVar:
		CurrLine = CurrLine->Next
	Loop

END SUB

SUB CompileWait (CompSub As SubType Pointer)

	'Time Intervals: us, 10us, ms, 10ms, s, m, h
	Dim As String InLine, Origin, Temp, Value, Unit, Condition
	Dim As Integer UP, T, Cycles, DS
	Dim As LinkedListElement Pointer CurrLine, NewCode
	
	FoundCount = 0
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = UCase(CurrLine->Value)
		
		IF Left(InLine, 5) = "WAIT " THEN
			
			Origin = ""
			IF INSTR(InLine, ";?F") <> 0 THEN
				Origin = Mid(InLine, INSTR(InLine, ";?F"))
				InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
			END IF
			
			Value = Mid(InLine, 6)
			IF Left(UCase(Value), 6) = "WHILE " OR Left(UCase(Value), 6) = "UNTIL " THEN
				Unit = LCase(Mid(Value, INSTR(Value, " ") + 1))
				Value = UCase(Left(Value, INSTR(Value, " ") - 1))
			Else
				Value = Trim(Value)
				FOR UP = LEN(Value) TO 1 STEP -1
					IF Mid(Value, UP, 1) = " " THEN
						Unit = LCase(Trim(Mid(Value, UP + 1)))
						Value = Trim(Left(Value, UP - 1))
						EXIT FOR
					END IF
				NEXT
			END IF
			
			'Time-based wait
			IF Value <> "UNTIL" AND Value <> "WHILE" THEN
				IF INSTR(Unit, "second") <> 0 THEN Replace Unit, "second", "s"
				Select Case Unit
				Case "us", "usec":
					Unit = "Delay_US"
				Case "10us", "10usec":
					Unit = "Delay_10US"
				Case "ms", "msec":
					Unit = "Delay_MS"
				Case "10ms", "10msec":
					Unit = "Delay_10MS"
				Case "s", "sec":
					Unit = "Delay_S"
				Case "m", "min":
					Unit = "Delay_M"
				Case "h", "hour":
					Unit = "Delay_H"
				Case Else:
					LogError Message("NoDelayUnits"), Origin
					CurrLine = LinkedListDelete(CurrLine)
					GoTo EndWaitCompile
				End Select
				
				Temp = Unit: Replace Temp, "Delay_", ""
				
				'Add word variables (PIC only)
				'If ModePIC Then
					If Unit = "Delay_US" Then AddVar "SysWaitTempUS", "WORD", 1, 0, "REAL", Origin
					If Unit = "Delay_MS" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin
					'Delays which depend on Delay_MS
					If Unit = "Delay_10MS" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin
					If Unit = "Delay_S" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin
					If Unit = "Delay_M" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin
					If Unit = "Delay_H" Then AddVar "SysWaitTempMS", "WORD", 1, 0, "REAL", Origin
				'End If
				
				'Check for delay of 0
				IF Trim(Value) = "0" OR VAL(Value) < 0 THEN
					CurrLine = LinkedListDelete(CurrLine)
					GOTO EndWaitCompile
				END If
				'Check for delay with floating point value
				If InStr(Value, ".") <> 0 Then
					If Trim(Value) = Trim(Str(Val(Value))) Then
						LogError Message("FloatInDelay"), Origin
						CurrLine = LinkedListDelete(CurrLine)
						GoTo EndWaitCompile
					End If
				End If
				
				'Compile longer delays as subs
				IF Unit <> "Delay_US" OR Not IsConst(Value) Then
					CurrLine->Value = "SysWaitTemp" + Temp + "=" + Value + Origin
					CurrLine = LinkedListInsert(CurrLine, " call " + Unit)
					RequestSub(CompSub, Unit)
					
					'Generate error when using US delay on slow chips
					IF Unit = "Delay_US" AND USDelaysInaccurate THEN
						LogWarning Message("WarningUSDelay"), Origin
					END If
					
				'Compile microsecond delays as inline when given constant length
				Else
					If ModePIC Then
						Cycles = MakeDec(Value) * ChipMHz / 4
					ElseIf ModeAVR Then
						Cycles = MakeDec(Value) * ChipMHz
					End If
					NewCode = GenerateExactDelay(Cycles)
					
					CurrLine = LinkedListDelete(CurrLine)
					CurrLine = LinkedListInsertList(CurrLine, NewCode)
					
				END IF
				
				FoundCount = FoundCount + 1
				
			'Condition-based Wait
			Else
				Condition = UCase(Unit)
				WSC = WSC + 1
				CurrLine->Value = "SysWaitLoop" + Str(WSC) + LabelEnd
				'Get code
				Temp = "TRUE": IF Value = "UNTIL" THEN Temp = "FALSE"
				NewCode = CompileConditions(Condition, Temp, Origin)
				'Add
				CurrLine = LinkedListInsertList(CurrLine, NewCode)
				'Append appropriate goto
				If Condition <> "AlwaysFalse" Then
					If ModePIC Then CurrLine = LinkedListInsert(CurrLine, " goto SysWaitLoop" + Str(WSC))
					If ModeAVR Then CurrLine = LinkedListInsert(CurrLine, " rjmp SysWaitLoop" + Str(WSC))
				End If
				
				FoundCount = FoundCount + 1
			END IF
			
		END IF
		
	EndWaitCompile:
		CurrLine = CurrLine->Next
	Loop
	
END SUB

Function CompileWholeArray (InLine As String, Origin As String) As LinkedListElement Pointer
	
	Dim As String DestVar, Source, Temp
	Dim As String ArrayHandler, DestArrayHandler, ArrayName, DestArrayName
	Dim StringTemp(250) As String
	Dim As Integer AF, ArrayType, TDC, PD
	Dim As LinkedListElement Pointer OutList, CurrLine
	Dim As SubType Pointer CurrSub
	
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Get subroutine
	CurrSub = Subroutine(GetSubID(Origin))
	
	'Find array address, display error if not found
	DestVar = Trim(Left(InLine, INSTR(InLine, "=") - 1))
	Source = Trim(Mid(InLine, INSTR(InLine, "=") + 1))
	
	'Set indirect pointer
	If ModePIC Then
		If ChipFamily = 15 Or ChipFamily = 16 Then
			CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, 0, CurrSub, Origin))
		Else
			CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, -1, CurrSub, Origin))
		End If
	ElseIf ModeAVR Then
		CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(DestVar, 0, CurrSub, Origin))
	End If
	
	'Get source data
	TDC = 0
	Do While INSTR(Source, ",") <> 0
		TDC += 1
		StringTemp(TDC) = Trim(Left(Source, INSTR(Source, ",") - 1))
		Source = Mid(Source, INSTR(Source, ",") + 1)
	Loop
	TDC += 1
	StringTemp(TDC) = Trim(Source)
	StringTemp(0) = Str(TDC)
	
	'Store source data
	For PD = 0 to TDC
		If ModePIC Then
			If ChipFamily = 12 Or ChipFamily = 14 Then
				'CurrLine = LinkedListInsert(CurrLine, "INDF = " + StringTemp(PD))
				CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(StringTemp(PD), "INDF", Origin))
				If PD < TDC Then CurrLine = LinkedListInsert(CurrLine, " incf FSR,F")
			ElseIf ChipFamily = 15 Then
				CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(StringTemp(PD), "INDF0", Origin))
				If PD < TDC Then CurrLine = LinkedListInsert(CurrLine, " incf FSR0L,F")
			ElseIf ChipFamily = 16 Then
				'CurrLine = LinkedListInsert(CurrLine, "POSTINC0 = " + StringTemp(PD))
				CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(StringTemp(PD), "POSTINC0", Origin))
			End If
		ElseIf ModeAVR Then
			
		End If
	Next
	
	Return OutList
End Function

Function ConfigNameMatch(ConfigIn As String, ConfigNameIn As String) As Integer
	'Checks whether an input config setting (ie MCLR_OFF) matches a name (ie MCLR)
	Dim As String Config, ConfigName
	Config = ConfigIn
	ConfigName = ConfigNameIn
	ReplaceAll Config, "_", " "
	ReplaceAll ConfigName, "_", " "
	
	'If = found, get whatever is before it
	If InStr(Config, "=") <> 0 Then Config = Trim(Left(Config, InStr(Config, "=") - 1))
	If InStr(ConfigName, "=") <> 0 Then ConfigName = Trim(Left(ConfigName, InStr(ConfigName, "=") - 1))
	
	'If name of setting in input config, name matches
	If WholeINSTR(Config, ConfigName) = 2 Then Return -1
	If WholeINSTR(ConfigName, Config) = 2 Then Return -1
	
	Dim As LinkedListElement Pointer CurrItem, Look1, Look2
	
	'Find equivalent names
	CurrItem = EqConfigSettings->Next
	Do While CurrItem <> 0
		
		'Look at every item in the list
		'Compare to every other item in the list
		Look1 = CurrItem->Metadata
		Look1 = Look1->Next
		Do While Look1 <> 0
			Look2 = CurrItem->Metadata
			Look2 = Look2->Next
			Do While Look2 <> 0
				If Look1 <> Look2 Then
					'Have two different but equivalent settings, check for them
					If ConfigName = Look1->Value And WholeINSTR(Config, Look2->Value) = 2 Then Return -1
					
				End If
				Look2 = Look2->Next
			Loop
			Look1 = Look1->Next
		Loop
		
		CurrItem = CurrItem->Next
	Loop
	
	If ConfigName = "OSC" And InStr(Config, "OSC") <> 0 Then Return -1
	
	'No match found
	Return 0
End Function

Function ConfigValueMatch(ConfigIn As String, ConfigValueIn As String) As Integer
	'Checks whether an input config setting (ie MCLR_OFF) matches a value (ie OFF)
	Dim As String Config, ConfigValue, CurrValue
	Dim As Integer CurrOption
	Config = ConfigIn
	ConfigValue = ConfigValueIn
	ReplaceAll Config, "_", " "
	ReplaceAll ConfigValue, "_", " "
	
	'If = in value, remove it and anything before it
	'(ie convert OSC = HS to HS)
	If InStr(ConfigValue, "=") <> 0 Then ConfigValue = Trim(Mid(ConfigValue, InStr(ConfigValue, ("=")) + 1))
	If InStr(Config, "=") <> 0 Then Config = Trim(Mid(Config, InStr(Config, ("=")) + 1))
	
	'Return -2 for best match
	If Config = ConfigValue Then Return -2
	
	'If value of setting in input config, value matches
	If WholeINSTR(Config, ConfigValue) = 2 Then Return -1
	If WholeINSTR(ConfigValue, Config) = 2 Then Return -1
	
	If Config = "DISABLED" And ConfigValue = "OFF" Then Return -1
	If Config = "ENABLED" And ConfigValue = "ON" Then Return -1
	
	'MCLR on some newer PICs
	If Config = "INTMCLR" And ConfigValue = "OFF" Then Return -1
	If Config = "EXTMCLR" And ConfigValue = "ON" Then Return -1
	
	'Internal oscillator
	If ConfigValue = "INT" Or ConfigValue = "INT IO" Then
		
		'Use non-clockout mode
		If WholeINSTR(Config, "CLKOUT") = 2 Then Return 0
		If WholeINSTR(Config, "IRCCLKOUT") = 2 Then Return 0
		
		'Possible matches
		If InStr(Config, "INTRC IO") <> 0 Then Return -1
		If InStr(Config, "INTRC OSC") <> 0 Then Return -1
		If Config = "INTRC" Then Return -1
		'If "INTOSC" found, make sure it's the option with IO
		If InStr(Config, "INTOSC") <> 0 Then
			If InStr(Config, "IO") <> 0 Then Return -1
			'This option doesn't have IO, return false if one with IO is found
			For CurrOption = 1 To COC
				If InStr(ConfigOps(CurrOption).OP, "IO") <> 0 Then
					Return 0
				End If
			Next
			Return -1
		End If
		If InStr(Config, "INTIO67") <> 0 Then Return -1
		If InStr(Config, "INTIO2") <> 0 Then Return -1
		If InStr(Config, "IRCIO67") <> 0 Then Return -1
		If InStr(Config, "IRC") <> 0 Then Return -1
	End If
	
	'No match found
	Return 0

End Function

Sub CreateCallTree
	
End Sub

Sub DisplayCallTree
	
	'Debugging sub
	'Will show all calls in program
	
	Dim CurrSub As Integer
	Dim CurrCall As LinkedListElement Pointer
	Dim CalledSub As SubType Pointer
	
	For CurrSub = 0 To SBC
		If Subroutine(CurrSub)->Required Then
			Print Subroutine(CurrSub)->Name
			
			CurrCall = Subroutine(CurrSub)->CallList->Next
			Do While CurrCall <> 0
				If CurrCall->MetaData <> 0 Then
					CalledSub = CurrCall->MetaData
					Print , CalledSub->Name
				End If
				
				CurrCall = CurrCall->Next
			Loop
			Print
			
		End If
	Next
	
End Sub

Sub ExtAssembler
	Dim As String DataSource, ErrFile
	Dim As Integer PD, Result
	
	AsmExe = ReplaceToolVariables(AsmExe,,, AsmTool)
	AsmParams = ReplaceToolVariables(AsmParams, "asm",, AsmTool)
	IF VBS = 1 THEN PRINT SPC(5); Message("Calling") + AsmExe
	Result = Exec(AsmExe, AsmParams)
	'SHELL MakeASM
	
	'Check that the assembly was successful
	IF OPEN(Left(OFI, Instr(OFI, ".")) + "err" For Input As #1) = 0 THEN
		DO WHILE NOT EOF(1)
			LINE INPUT #1, DataSource
			IF INSTR(UCase(DataSource), "ERROR") <> 0 THEN
				LogError "ASM: " + Trim(DataSource)
			END IF
		LOOP
		CLOSE
		
	ElseIf Result = -1 Then
		LogError Message("AssemblerNotFound")
	END IF
	
	'Get name of hex file to download
	HFI = OFI
	For PD = Len(HFI) To 1 Step -1
		If Mid(HFI, PD, 1) = "." Then
			HFI = Left(HFI, PD) + "hex"
			Exit For
		End If
	Next
	
End Sub

Sub ExtractParameters(ByRef NewSubCall As SubCallType, InLineCopy As String, Origin As String)
	
	Dim As String TrimParams, SubName, SubSig, Temp
	Dim As Integer FP, PD
	
	'Clear parameter list
	With NewSubCall
		.CalledID = 0
		.Params = 0
	End With
	
	'Get origin (if not set, but present in input line)
	IF INSTR(InLineCopy, ";?F") <> 0 THEN 
		If Origin = "" Then Origin = Mid(InLineCopy, INSTR(InLineCopy, ";?F"))
		InLineCopy = RTrim(Left(InLineCopy, INSTR(InLineCopy, ";?F") - 1))
	END If
	
	'Get parameters
	'Find sub calls with parameters in program
	TrimParams = InLineCopy
	SubName = ReplaceFnNames(TrimParams)
	SubSig = ""
	IF INSTR(InLineCopy, "(") <> 0 OR INSTR(TrimParams, " ") <> 0 THEN
		
		'Get parameters
		'Add brackets
		FP = INSTR(InLineCopy, " ") + 1
		IF INSTR(TrimParams, " ") <> 0 AND (INSTR(InLineCopy, "(") = 0 OR INSTR(InLineCopy, "(") > FP) AND (Mid(InLineCopy, FP + 1, 1) <> "(") THEN
			Temp = Left(InLineCopy, INSTR(InLineCopy, " "))
			Replace InLineCopy, Temp, Temp + "("
			InLineCopy = InLineCopy + ")"
		END If
		
		'Get sub name
		SubName = Left(InLineCopy, INSTR(InLineCopy, "(") - 1)
		
		'Get sub parameters
		TrimParams = Mid(InLineCopy, INSTR(InLineCopy, "(") + 1)
		IF TrimParams = ")" THEN GOTO NoSubParams
		TrimParams = Trim(TrimParams)
		IF Right(TrimParams, 1) = ")" THEN TrimParams = RTrim(Left(TrimParams, LEN(TrimParams) - 1))
		
		With NewSubCall
			
			'Get Origin
			.Origin = Origin
			
			'Get parameters
			'Need to keep function names hidden, or strange things happen
			DO WHILE INSTR(TrimParams, ",") <> 0
				.Params += 1
				.Param(.Params, 1) = Trim(Left(TrimParams, INSTR(TrimParams, ",") - 1))
				.Param(.Params, 2) = TypeOfValue(ReplaceFnNames(.Param(.Params, 1)), Subroutine(GetSubID(Origin)), -1)
				'Print .Param(.Params, 1) + " is a " + .Param(.Params, 2)
				SubSig += GetTypeLetter(.Param(.Params, 2))
				TrimParams = LTrim(Mid(TrimParams, INSTR(TrimParams, ",") + 1))
			LOOP
			If TrimParams <> "" Then
				.Params += 1
				.Param(.Params, 1) = TrimParams
				.Param(.Params, 2) = TypeOfValue(ReplaceFnNames(.Param(.Params, 1)), Subroutine(GetSubID(Origin)), -1)
				'Print .Param(.Params, 1) + " is a " + .Param(.Params, 2)
				SubSig += GetTypeLetter(.Param(.Params, 2))
			End If
			'Print
			.CallSig = SubSig
		End With
		
	Else
		With NewSubCall
			.Params = 0
			.CallSig = ""
		End With
	End If
	NoSubParams:
	
End Sub

Sub FinalOptimise
	
	'Replace call with rcall where possible (AVR and 18F)
	OptimiseCalls
	
	'Perform final optimisation on code, before writing assembly
	OptimiseIF
	
	'Check for uncompiled lines
	FindUncompiledLines
	
End Sub

Sub FindAssembly (CompSub As SubType Pointer)
	Dim As String Temp, CalledSub
	Dim As Integer PD, T, CallPos, RCallPos
	Dim As LinkedListElement Pointer CurrLine
	
	CallPos = IsASM("call")
	RCallPos = IsASM("rcall")
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		'If "ASM" is at start of line, remove it
		
		IF Left(CurrLine->Value, 4) = "ASM " THEN
			CurrLine->Value = Trim(Mid(CurrLine->Value, 5))
			CurrLine->Value = " " + LCase(Left(CurrLine->Value, LEN(Temp))) + Mid(CurrLine->Value, LEN(Temp) + 1)
			IF INSTR(CurrLine->Value, ";?") <> 0 THEN CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?") - 1))
		Else
			T = IsASM(CurrLine->Value)
			If T <> 0 AND Left(CurrLine->Value, 1) <> " " THEN
				Temp = Trim(ASMCommands(T).Syntax)
				IF INSTR(Temp, " ") <> 0 THEN Temp = Trim(Left(Temp, INSTR(Temp, " ") - 1))
				
				'Record calls
				If T = CallPos Or T = RCallPos Then
					CalledSub = Trim(Mid(CurrLine->Value, LEN(Temp) + 1))
					IF InStr(CalledSub, ";?") <> 0 THEN CalledSub = RTrim(Left(CalledSub, INSTR(CalledSub, ";?") - 1))
					RequestSub(CompSub, CalledSub, "")
				End If
				
				CurrLine->Value = " " + LCase(Left(CurrLine->Value, LEN(Temp))) + Mid(CurrLine->Value, LEN(Temp) + 1)
				
				IF InStr(CurrLine->Value, ";?") <> 0 THEN CurrLine->Value = RTrim(Left(CurrLine->Value, INSTR(CurrLine->Value, ";?") - 1))
			End If
		End If
		
		CurrLine = CurrLine->Next
	Loop
End Sub

Function FindPotentialBanks(CurrLine As LinkedListElement Pointer, OutList As LinkedListElement Pointer, CheckedLines As LinkedListElement Pointer) As LinkedListElement Pointer
	'Will accept a code line and an optional output list
	'Will check to see what bank line will use, and add it to the list
	'Need to keep list of lines that have been checked to avoid infinite loops
	
	Dim As Integer CreatedCheckedList, LineNotChecked, BankIsNew
	Dim As LinkedListElement Pointer CurrPrev, CurrPrevPos, CheckListPos, OutListPos
	Dim As ProgLineMeta Pointer CurrMeta, CheckMeta
	
	'Make sure output list exists
	If OutList = 0 Then
		OutList = LinkedListCreate
	End If
	
	'Make sure check lines list exists
	CreatedCheckedList = 0
	If CheckedLines = 0 Then
		CreatedCheckedList = -1
		CheckedLines = LinkedListCreate
	End If
	
	'Check to see if line requires a bank
	CurrMeta = CurrLine->MetaData
	If CurrMeta <> 0 Then
		
		'Check all previous lines to see if they set a bank
		'MetaData in PrevCommands points to line
		'If MetaData in PrevCommands points to 0, line is first in sub
		CurrPrevPos = CurrMeta->PrevCommands->Next
		Do While CurrPrevPos <> 0
			
			CurrPrev = CurrPrevPos->MetaData
			
			'Is first line in sub?
			If CurrPrev = 0 Then
				'If so, it will always be in bank 0
				BankIsNew = -1
				OutListPos = OutList->Next
				Do While OutListPos <> 0
					If OutListPos->NumVal = 0 Then
						BankIsNew = 0
						Exit Do
					End If
					OutListPos = OutListPos->Next
				Loop
				If BankIsNew Then
					OutListPos = LinkedListInsert(OutList, "")
					OutListPos->NumVal = 0
				End If
				
			Else
				'Has line been checked already?
				LineNotChecked = -1
				CheckListPos = CheckedLines->Next
				Do While CheckListPos <> 0
					If CheckListPos->MetaData = CurrPrev Then
						LineNotChecked = 0
						Exit Do
					End If
					CheckListPos = CheckListPos->Next
				Loop
				'Line not checked, so check it and record the check
				If LineNotChecked Then
					LinkedListInsert(CheckedLines, CurrPrev)
					
					CheckMeta = CurrPrev->MetaData
					If CheckMeta <> 0 Then
						If CheckMeta->RequiredBank <> -1 Then
							'Have found a bank, add to the bank list
							BankIsNew = -1
							OutListPos = OutList->Next
							Do While OutListPos <> 0
								If OutListPos->NumVal = CheckMeta->RequiredBank Then
									BankIsNew = 0
									Exit Do
								End If
								OutListPos = OutListPos->Next
							Loop
							If BankIsNew Then
								OutListPos = LinkedListInsert(OutList, "")
								OutListPos->NumVal = CheckMeta->RequiredBank
							End If
							
						Else
							'Check lines before current previous line
							OutList = FindPotentialBanks(CurrPrev, OutList, CheckedLines)
							
						End If
					End If
				End If
			
			End If
			
			CurrPrevPos = CurrPrevPos->Next
		Loop
		
	End If
	
	'Delete checked lines list if created by this sub
	If CreatedCheckedList Then
		LinkedListDelete(CheckedLines)
	End If
	
	Return OutList
End Function

Sub FindUncompiledLines
	'Search the compiler output listing for uncompiled lines
	'Show error where found
	
	Dim As LinkedListElement Pointer CurrLine
	Dim As String TempLine, Origin
	
	CurrLine = CompilerOutput->CodeList->Next
	Do While CurrLine <> 0
		IF INSTR(CurrLine->Value, ";?F") <> 0 THEN
			Origin = Mid(CurrLine->Value, INSTR(CurrLine->Value, ";?F"))
			TempLine = Trim(Left(CurrLine->Value, InStr(CurrLine->Value, ";?F") - 1))
			
			'Recognise some block ends
			'These are typically dealt with when the start is found
			'If left in, there's a missing start somewhere!
			If TempLine = "END REPEAT" Then
				LogError Message("EndWithoutRepeat"), Origin
			ElseIf TempLine = "LOOP" Or Left(TempLine, 5) = "LOOP " Then
				LogError Message("LoopWithoutDo"), Origin
			ElseIf TempLine = "ELSE" Then
				LogError Message("ElseWithoutIf"), Origin
			ElseIf TempLine = "NEXT" Then
				LogError Message("NextWithoutFor"), Origin
				
			'Anything else uncompiled at this point is a syntax error
			Else
				LogError Message("SynErr"), Origin
			End If
		END If
		CurrLine = CurrLine->Next
	Loop
End Sub

Function FixBit (InBit As String, Origin As String) As String
	'Fix any references to bits > 7
	
	Dim As String DivChar, VarName, VarType, VarBit, Temp
	Dim As Integer B
	
	'Get dividing character
	DivChar = "."
	IF INSTR(InBit, ".") = 0 AND INSTR(InBit, ",") <> 0 Then DivChar = ","
	
	'Exit if not a bit
	IF INSTR(InBit, DivChar) = 0 Then Return Inbit
	
	VarName = Trim(Left(InBit, INSTR(InBit, DivChar) - 1))
	VarBit = Trim(Mid(InBit, INSTR(InBit, DivChar) + 1))
	VarType = TypeOfVar(VarName, Subroutine(GetSubID(Origin)))
	B = VAL(VarBit)
	
	'If bit < 8, return without change
	If B < 8 Then Return InBit
	
	'Prepare error
	Temp = Message("BadVarBit")
	Replace Temp, "%var%", VarName
	Replace Temp, "%type%", VarType
	Replace Temp, "%bit%", VarBit
	
	Select Case VarType
		Case "BYTE":
			LogError Temp, Origin
		Case "WORD", "INTEGER":
			If B < 16 Then Return VarName + "_H" + DivChar + Trim(Str(B - 8))
			LogError Temp, Origin
		Case "LONG":
			If B < 16 Then Return VarName + "_H" + DivChar + Trim(Str(B - 8))
			If B < 24 Then Return VarName + "_U" + DivChar + Trim(Str(B - 16))
			If B < 32 Then Return VarName + "_E" + DivChar + Trim(Str(B - 24))
			LogError Temp, Origin
		Case Else:
			'Error, but should be found elsewhere
	End Select
	
	'Return unchanged
	Return InBit
	
End Function

SUB FixFunctions(CompSub As SubType Pointer)

	'Fix functions that are used by DO and WAIT, so they are updated inside
	'the loops
	Dim As String SubName
	Dim As Integer T, CD
	Dim As LinkedListElement Pointer CurrLine, FindStart
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		
		If Left(CurrLine->Value, 9) = " call FN_" THEN
			IF Left(CurrLine->Next->Value, 11) = "SysWaitLoop" OR Left(CurrLine->Next->Value, 11) = "SysDoLoop_S" THEN
				SubName = Mid(CurrLine->Value, 10)
				FindStart = CurrLine->Prev
				Do While FindStart <> 0
					
					IF FindStart->Value = ";FNSTART," + SubName THEN
						FindStart->Value = CurrLine->Next->Value
						LinkedListDelete(CurrLine->Next)
						CurrLine = FindStart->Prev
						Exit Do
					END If
					
					FindStart = FindStart->Prev
				Loop
			'Not inside a loop, no change needed, just delete the ;FNSTART
			Else
				SubName = Mid(CurrLine->Value, 10)
				FindStart = CurrLine->Prev
				Do While FindStart <> 0
					IF FindStart->Value = ";FNSTART," + SubName THEN
						LinkedListDelete(FindStart)
						Exit Do
					END If
					FindStart = FindStart->Prev
				Loop
			END If
			
		ElseIf Left(CurrLine->Value, 10) = ";BLOCKEND," THEN
			IF Left(CurrLine->Next->Value, 11) = "SysWaitLoop" OR Left(CurrLine->Next->Value, 11) = "SysDoLoop_S" THEN
				SubName = Mid(CurrLine->Value, 11)
				FindStart = CurrLine->Prev
				Do While FindStart <> 0
					
					IF FindStart->Value = ";BLOCKSTART," + SubName THEN
						FindStart->Value = CurrLine->Next->Value
						LinkedListDelete(CurrLine->Next)
						CurrLine = LinkedListDelete(CurrLine)
						Exit Do
					END IF
					FindStart = FindStart->Prev
				Loop
			END IF
		
		END IF
		CurrLine = CurrLine->Next
	Loop
	
	
END SUB

Sub FixPointerOps (CompSub As SubType Pointer)
	
	'AVR Only
	
	'Replace operations on the SysPointerX pseudo variable with operations
	'using ld X/st x.
	
	Dim As String DataSource, DataReg
	Dim As LinkedListElement Pointer CurrLine 
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		If WholeINSTR(CurrLine->Value, "SysPointerX") = 2 Then
			DataSource = CurrLine->Value
			
			Select Case LCase(Left(Trim(DataSource), 3))
				Case "lds":
				DataReg = Mid(Trim(DataSource), 4)
				DataReg = Trim(Left(DataReg, Instr(DataReg, ",") - 1))
				CurrLine->Value = " ld " + DataReg + ",X"
				
				Case "sts":
				DataReg = Trim(Mid(DataSource, Instr(DataSource, ",") + 1))
				CurrLine->Value = " st X," + DataReg
				
				Case Else:
				LogError "Internal error in FixPointerOps"
				
			End Select
		End If
		
		CurrLine = CurrLine->Next
	Loop
	
End Sub

Sub FixSinglePinSet
	'Deal with pin direction setting on 12 bit PIC
	'Need to cache W into TRISIO before tris
	'Also need to copy TRISIO into tris after bcf/bsf on TRISIO
	'Need to operate on every subroutine
	
	Dim As Integer CurrSub, NextSamePort, SearchPos, FoundPos
	Dim As LinkedListElement Pointer CurrLine
	Dim As String Port, NextPort, TrisPort
	Dim As SubType Pointer InitSys
	
	'12 bit PIC only
	If ChipFamily <> 12 Then Exit Sub
	
	'Only needed if individual pin dir is set
	If PinDirShadows = 0 Then Exit Sub
	
	'Check each sub
	For CurrSub = 0 To SBC
		With *Subroutine(CurrSub)
			If .Required And .Compiled Then
				
				'Check each line of sub
				CurrLine = .CodeStart->Next
				Do While CurrLine <> 0
					
					'Find tris x
					If Left(CurrLine->Value, 6) = " tris " Then
						'Get port
						Port = Trim(Mid(CurrLine->Value, 7))
						
						'Get tris cache
						TrisPort = ""
						If Port = "GPIO" Then
							TrisPort = "IO"
						ElseIf Left(Port, 4) = "PORT" And Len(Port) = 5 Then
							TrisPort = Mid(Port, 5)
						End If
						
						'Check if shadow needed
						FoundPos = -1
						For SearchPos = 1 To PinDirShadows
							If PinDirShadow(SearchPos) = TrisPort Then
								FoundPos = SearchPos
								Exit For
							End If
						Next
						
						If FoundPos <> -1 Then
							'Set buffer
							LinkedListInsert(CurrLine->Prev, " movwf TRIS" + TrisPort)
							
							'Add caching instructions
							AddVar("TRIS" + TrisPort, "BYTE", 1, 0, "REAL", "")
						End If  
						
					'Find bcf/bsf TRISx
					ElseIf Left(CurrLine->Value, 9) = " bsf TRIS" Or Left(CurrLine->Value, 9) = " bcf TRIS" Then
						'Print "Found single dir set:", CurrLine->Value
						
						'Get port
						Port = Mid(CurrLine->Value, 10)
						If InStr(Port, ",") <> 0 Then Port = Left(Port, InStr(Port, ",") - 1)
						Port = Trim(Port)
						
						'If there is a block of dir commands, get last one for same port
						Do
							NextSamePort = 0
							
							'Is there another line?
							If CurrLine->Next <> 0 Then
								'Is the next line a tris setting instruction?
								If Left(CurrLine->Next->Value, 9) = " bsf TRIS" Or Left(CurrLine->Next->Value, 9) = " bcf TRIS" Then
									'What port does it set?
									NextPort = Mid(CurrLine->Value, 10)
									If InStr(NextPort, ",") <> 0 Then NextPort = Left(NextPort, InStr(NextPort, ",") - 1)
									NextPort = Trim(NextPort)
									'Is it the same?
									If NextPort = Port Then
										CurrLine = CurrLine->Next
										NextSamePort = -1
									End If
								End If
							End If
						Loop While NextSamePort
						
						If Len(Port) = 1 Or Port = "IO" Then
							'Calculate port name
							If Len(Port) = 1 Then
								TrisPort = "PORT" + Port
							Else
								TrisPort = "GPIO"
							End If
							
							'Add caching instructions
							AddVar("TRIS" + Port, "BYTE", 1, 0, "REAL", "")
							CurrLine = LinkedListInsert(CurrLine, " movf TRIS" + Port + ",W")
							CurrLine = LinkedListInsert(CurrLine, " tris " + TrisPort)  
							
						End If
						
					End If
					
					CurrLine = CurrLine->Next
				Loop
				
			End If
		End With
	Next
	
	'Add pin dir shadow init code to end of InitSys
	If PinDirShadows > 0 Then
		SearchPos = LocationOfSub("InitSys", "")
		If SearchPos > 0 Then
			InitSys = Subroutine(SearchPos)
			If InitSys <> 0 Then
				
				'Get last line of sub
				CurrLine = InitSys->CodeStart
				Do While CurrLine->Next <> 0
					CurrLine = CurrLine->Next
				Loop
				
				'Add buffer setting code
				CurrLine = LinkedListInsert(CurrLine, " movlw 255")
				For SearchPos = 1 To PinDirShadows
					CurrLine = LinkedListInsert(CurrLine, " movwf TRIS" + PinDirShadow(SearchPos))
				Next
				
			End If 
		End If
	End If
	
End Sub

Sub FreeCalcVar (VarName As String)
	'Mark a calc var as available for reuse
	Dim As String Temp
	Dim As Integer CV, CD
	
	If VarName <> "" Then
		'If name specified, only mark that var
		
		'Exit if input is not a calc var
		IF Left(UCase(VarName), 7) <> "SYSTEMP" Then Exit Sub
		
		Temp = Mid(Trim(VarName), 8)
		CV = VAL(Temp)
		
		CalcVars(CV).Status = "A"
	Else
		'If no name, mark all
		For CD = 1 to TCVC
			CalcVars(CD).Status = "A"
		Next
	End If
End Sub

Function GenerateArrayPointerSet(DestVar As String, DestPtr As Integer, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer
	
	'DestPtr:
	' 0: SysStringA on 16F, FSR0 on enhanced 16F/18F
	' 1: SysStringB on 16F, FSR1 on enhanced 16F/18F
	'-1: FSR on 16F, FSR0 on 18F
	
	Dim As String ArrayHandler, Temp, ArrayName, WholePtrVar, DestPtrL, DestPtrH
	Dim As Integer ArrayType
	Dim As VariableType Pointer ArrayPtr
	
	'Create output list
	Dim As LinkedListElement Pointer CurrLine, OutList
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Get array name (no ( or $ at the end)
	ArrayName = Trim(UCase(DestVar))
	If INSTR(ArrayName, "(") <> 0 Then ArrayName = RTrim(Left(ArrayName, INSTR(ArrayName, "(") - 1))
	If INSTR(ArrayName, "$") <> 0 Then ArrayName = RTrim(Left(ArrayName, INSTR(ArrayName, "$") - 1))
	
	'Get array data, exit if not declared
	ArrayPtr = VarAddress(DestVar, CurrSub)
	IF ArrayPtr = 0 THEN
		Temp = Message("UndeclaredArray")
		Replace Temp, "%array%", ArrayName
		LogError Temp, Origin
		Return OutList
	END IF
	
	'Find array type, get source location
	IF ArrayPtr->Pointer = "REAL" THEN
		ArrayHandler = ArrayPtr->Name
		ArrayType = 0
	ElseIF ArrayPtr->Pointer = "POINTER" THEN
		ArrayHandler = "Sys" + ArrayName + "Handler"
		AddVar ArrayHandler, "WORD", 1, CurrSub, "REAL", Origin
		ArrayType = 1
	Else
		Print "Unknown array type, "; ArrayPtr->Name; " is "; ArrayPtr->Pointer
	END IF
	
	'Get dest pointer
	If ModePIC Then
		If ChipFamily = 15 Or ChipFamily = 16 Then
			If DestPtr = -1 Then DestPtr = 0
			DestPtrL = "FSR" + Str(DestPtr) + "L"
			DestPtrH = "FSR" + Str(DestPtr) + "H"
		Else
			If DestPtr = -1 Then
				WholePtrVar = "FSR"
			ElseIf DestPtr = 0 Then
				WholePtrVar = "SysStringA"
			ElseIf DestPtr = 1 Then
				WholePtrVar = "SysStringB"
			Else
				WholePtrVar = "SysStringC"
			End If
			AddVar WholePtrVar, "WORD", 1, 0, "REAL", Origin 'Needs to be global
			DestPtrL = GetByte(WholePtrVar, 0)
			DestPtrH = GetByte(WholePtrVar, 1)
		End If
	ElseIf ModeAVR Then
		If DestPtr = 0 Then
			WholePtrVar = "SysStringA"
		ElseIf DestPtr = 1 Then
			WholePtrVar = "SysStringB"
		Else
			WholePtrVar = "SysStringA"
		End If
		AddVar WholePtrVar, "WORD", 1, 0, "REAL", Origin 'Needs to be global
		DestPtrL = GetByte(WholePtrVar, 0)
		DestPtrH = GetByte(WholePtrVar, 1)
	ElseIf ModeZ8 Then
		
	End If
	
	If ModePIC Then
		'Warn about unsafe strings
		If ChipFamily = 12 Then
			LogWarning Message("WarningUnsafeString")
		End If
		
		If ChipFamily = 12 Or ChipFamily = 14 Then
			'DestPtr = -1: Set FSR directly
			If DestPtr = -1 Then
				'ArrayType = 0: Have a constant address
				If ArrayType = 0 Then
					CurrLine = LinkedListInsert(CurrLine, " movlw low " + ArrayHandler)
					CurrLine = LinkedListInsert(CurrLine, " movwf FSR")
					If HighFSR Then CurrLine = LinkedListInsert(CurrLine, " bankisel " + ArrayHandler)
				Else
					'ArrayType = 1: Have another pointer
					CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 0) + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf FSR")
					If HighFSR Then
						CurrLine = LinkedListInsert(CurrLine, " bcf STATUS, IRP")
						CurrLine = LinkedListInsert(CurrLine, " btfsc " + GetByte(ArrayHandler, 1) + ",0")
						CurrLine = LinkedListInsert(CurrLine, " bsf STATUS, IRP")
					End If
				End If
			Else
				If ArrayType = 0 Then
					CurrLine = LinkedListInsert(CurrLine, " movlw low " + ArrayHandler)
					CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
					CurrLine = LinkedListInsert(CurrLine, " movlw high " + ArrayHandler)
					CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
				Else
					CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 0) + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
					CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 1) + ",W")
					CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
				End If
			End If
		
		ElseIf ChipFamily = 15 Then
			If ArrayType = 0 Then
				CurrLine = LinkedListInsert(CurrLine, " movlw low " + ArrayHandler)
				CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
				CurrLine = LinkedListInsert(CurrLine, " movlw high " + ArrayHandler)
				CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
			Else
				CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 0) + ",W")
				CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrL)
				CurrLine = LinkedListInsert(CurrLine, " movf " + GetByte(ArrayHandler, 1) + ",W")
				CurrLine = LinkedListInsert(CurrLine, " movwf " + DestPtrH)
			End If
		
		ElseIf ChipFamily = 16 Then
			If ArrayType = 0 Then
				CurrLine = LinkedListInsert(CurrLine, " lfsr " + Str(DestPtr) + "," + ArrayHandler)
			Else
				CurrLine = LinkedListInsert(CurrLine, " movff " + GetByte(ArrayHandler, 0) + "," + DestPtrL)
				CurrLine = LinkedListInsert(CurrLine, " movff " + GetByte(ArrayHandler, 1) + "," + DestPtrH)
			End If
		End If
		
	ElseIf ModeAVR Then
		If ArrayType = 0 Then
			CurrLine = LinkedListInsert(CurrLine, " ldi " + DestPtrL + ",low(" + ArrayHandler + ")")
			CurrLine = LinkedListInsert(CurrLine, " ldi " + DestPtrH + ",high(" + ArrayHandler + ")")
		ElseIf ArrayType = 1 Then
			CurrLine = LinkedListInsertList(CurrLine, CompileVarSet(ArrayHandler, WholePtrVar, Origin)) 
		End If
	End If
	
	Return OutList
End Function

Function GenerateAutoPinDir As LinkedListElement Pointer
	'Generates automatic pin direction setting code
	'Returns a linked list containing the Dir commands
	
	Dim As LinkedListElement Pointer OutList, CurrLine, CurrPin, SearchLine
	Dim As PinDirType Pointer CurrPinDir, CurrPortDir
	Dim As String WholePortName, Temp
	
	'Create output list
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Get pin directions 
	'Print "Pin", "In", "Out", "Read", "Written"
	CurrPin = PinDirections->Next
	Do While CurrPin <> 0
		CurrPinDir = CurrPin->MetaData
		'Print CurrPin->Value, CurrPinDir->SetIn, CurrPinDir->SetOut, CurrPinDir->ReadFrom, CurrPinDir->WrittenTo
		
		'If direction not set manually, and required direction is known, set direction
		With (*CurrPinDir)
			'Checking a pin
			If InStr(CurrPin->Value, ".") <> 0 Then
								
				'If pin direction hasn't been set but is obvious, set it automatically
				If Not .SetIn And Not .SetOut Then
					
					'Make sure no conflicting whole port direction set used
					WholePortName = Left(CurrPin->Value, InStr(CurrPin->Value, ".") - 1)
					SearchLine = PinDirections->Next
					Do While SearchLine <> 0
						If SearchLine->Value = WholePortName Then
							'Have found entire port setting
							'If pin and port setting conflict, don't set
							CurrPortDir = SearchLine->MetaData
							'If whole port dir set, don't set automatically 
							If CurrPortDir->WholePort <> 0 Then GoTo SkipPinDirSet
							'If whole port set at all, don't set automatically
							If CurrPortDir->SetIn Or CurrPortDir->SetOut Then GoTo SkipPinDirSet
							'If whole port dir clashes with single bit dir, don't set
							If Not (CurrPortDir->ReadFrom Or .ReadFrom) Xor (CurrPortDir->WrittenTo Or .WrittenTo) Then
								'Pin directions not set, and cannot be guessed - show warning
								If .ReadFrom And .WrittenTo Then
									Temp = Message("WarningNoAutoPinDir")
									Replace Temp, "%pin%", CurrPin->Value
									LogWarning(Temp)
								End If
								GoTo SkipPinDirSet
							End If
							
							'No conflict, so stop looking for conflict and set dir if needed
							Exit Do
						End If
						SearchLine = SearchLine->Next
					Loop
					
					'Pin is read only
					If .ReadFrom Then
						CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " IN")
					'Pin is written only
					ElseIf .WrittenTo Then
						CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " OUT")
					End If
				End If
				
				SkipPinDirSet:
				
			'Checking a port
			Else
				
				'If pin direction hasn't been set but is obvious, set it automatically
				If (Not .SetIn And Not .SetOut And .WholePort = 0) And (.ReadFrom Xor .WrittenTo) Then
					
					'Make sure no conflicting single pin direction set used
					SearchLine = PinDirections->Next
					Do While SearchLine <> 0
						If Left(SearchLine->Value, Len(CurrPin->Value)) = CurrPin->Value Then
							'Have found single setting
							'Don't set port if any bits are set
							'Port is actually pin
							CurrPortDir = SearchLine->MetaData
							'If whole port set at all, don't set automatically
							If CurrPortDir->SetIn Or CurrPortDir->SetOut Then GoTo SkipPortDirSet
							'If whole port dir clashes with single bit dir, don't set
							If Not (CurrPortDir->ReadFrom Or .ReadFrom) Xor (CurrPortDir->WrittenTo Or .WrittenTo) Then GoTo SkipPortDirSet
							
						End If
						SearchLine = SearchLine->Next
					Loop
					
					If .ReadFrom Then
						CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " IN")
					ElseIf .WrittenTo Then
						CurrLine = LinkedListInsert(CurrLine, "DIR " + CurrPin->Value + " OUT")
					End If
				End If
				
				SkipPortDirSet:
			End If
			
		End With
		
		CurrPin = CurrPin->Next
	Loop

	Return OutList
End Function

Function GenerateBitSet(BitNameIn As String, NewStatus As String, Origin As String) As LinkedListElement Pointer
	
	Dim As String InLine, Temp, BitName
	Dim As String VarName, VarType, VarBit, Status, VarNameOld, VarBitOld
	
	Dim As LinkedListElement Pointer OutList, CurrLine
	
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Get Bit var and name
	BitName = BitNameIn
	'If no var, might be dealing with an SFR bit
	If InStr(BitName, ".") = 0 Then
		BitName = GetWholeSFR(BitName)
	End If
	'Should now have a var!
	If InStr(BitName, ".") <> 0 Then
		VarName = Left(BitName, InStr(BitName, ".") - 1)
		VarBit = Mid(BitName, InStr(BitName, ".") + 1)
	Else
		Return OutList
	End If
	Status = NewStatus
	VarType = TypeOfVar(VarName, Subroutine(GetSubID(Origin)))
	VarNameOld = VarName: VarBitOld = VarBit
	
	'Show error if used on invalid type
	Select Case VarType
	Case "BYTE", "WORD", "INTEGER", "LONG":
		'Do nothing
	Case Else
		'Show error
		Temp = Message("BadCommandType")
		Replace Temp, "%command%", "SET"
		Replace Temp, "%type%", LCase(VarType)
		LogError Temp, Origin
		Return OutList
	End Select
	
	'If bit > 7, operate on high byte
	IF Val(VarBit) > 7 And (VarType = "WORD" Or VarType = "INTEGER") THEN
		VarBit = Str(VAL(VarBit) - 8)
		VarName = VarName + "_H"
	
	ElseIf Val(VarBit) > 7 And VarType = "LONG" Then
		If Val(VarBit) <= 15 Then
			VarBit = Str(VAL(VarBit) - 8)
			VarName = VarName + "_H"
		ElseIf Val(VarBit) <= 23 Then
			VarBit = Str(VAL(VarBit) - 16)
			VarName = VarName + "_U"
		ElseIf Val(VarBit) <= 31 Then
			VarBit = Str(VAL(VarBit) - 24)
			VarName = VarName + "_E"
		End If
	End If
	
	'If bit still > 7, bit is wrong
	If Val(VarBit) > 7 Then
		Temp = Message("BadVarBit")
		Replace Temp, "%var%", VarNameOld
		Replace Temp, "%type%", LCase(VarType)
		Replace Temp, "%bit", VarBitOld
		LogError Temp, Origin
		Return OutList
	End If
	
	'Status should be 0 or 1
	If Status <> "0" And Status <> "1" Then
		Temp = Message("BadSetStatus")
		Replace Temp, "%status%", Status
		LogError Temp, Origin
		Return OutList
	End If
	
	If ModePIC Then
		Temp = " bsf ": IF Status = "0" THEN Temp = " bcf "
		CurrLine = LinkedListInsert(CurrLine, Temp + VarName + "," + VarBit)
		If Not IsIOReg(VarName) Then AddVar VarName, "BYTE", 1, 0, "REAL", Origin
	
	ElseIf ModeAVR Then
		IF VarName = "SREG" Then
			Temp = " se": IF Status = "0" THEN Temp = " cl"
			CurrLine = LinkedListInsert(CurrLine, Temp + LCase(Trim(VarBit)))
		ElseIf IsRegister(VarName) Then
			Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
			CurrLine = LinkedListInsert(CurrLine, Temp + " " + VarName + ",1<<" + VarBit)
			
		ElseIf IsLowIOReg(VarName) Then
			Temp = " sbi ": IF Status = "0" THEN Temp = " cbi "
			CurrLine = LinkedListInsert(CurrLine, Temp + VarName + "," + VarBit)
			
		ElseIf IsIOReg(VarName) Then
			Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
			CurrLine = LinkedListInsert(CurrLine, " in SysValueCopy," + VarName)
			CurrLine = LinkedListInsert(CurrLine, Temp + "SysValueCopy,1<<" + VarBit)
			CurrLine = LinkedListInsert(CurrLine, " out " + VarName + ",SysValueCopy")
			AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
			
		Else
			Temp = " sbr ": IF Status = "0" THEN Temp = " cbr "
			CurrLine = LinkedListInsert(CurrLine, " lds SysValueCopy," + VarName)
			CurrLine = LinkedListInsert(CurrLine, Temp + "SysValueCopy,1<<" + VarBit)
			CurrLine = LinkedListInsert(CurrLine, " sts " + VarName + ",SysValueCopy")
			AddVar "SysValueCopy", "BYTE", 1, 0, "REAL", ""
			AddVar VarName, "BYTE", 1, 0, "REAL", Origin
		End If
	
	ElseIf ModeZ8 Then
		If Status = "0" Then
			CurrLine = LinkedListInsert(CurrLine, " orx " + VarName + ", #(1 <<" + Trim(VarBit) + ")" )
		Else
			CurrLine = LinkedListInsert(CurrLine, " andx " + VarName + ", #!(1 <<" + Trim(VarBit) + ")")
		End If
		
	End If
	
	Return OutList
End Function

Function GenerateExactDelay(ByVal Cycles As Integer) As LinkedListElement Pointer
	
	Dim As Integer DS ', AddDelayTemp, AddDelayTemp2
	Dim As LinkedListElement Pointer OutList, CurrPos
	OutList = LinkedListCreate
	CurrPos = OutList
	
	'AddDelayTemp = 0
	Do
		If ModePIC Then
			If Cycles >= 772 Then
				DS = (Cycles - 1) \ 771
				If DS > 255 Then DS = 255
				Cycles -= 1 + DS * 771
				CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP2 = " + Str(DS))
				CurrPos = LinkedListInsert(CurrPos, "DelayUSO" + Str(USDC))
				CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = 0")
				CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC))
				CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP,F")
				CurrPos = LinkedListInsert(CurrPos, " goto DelayUS" + Str(USDC))
				CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP2,F")
				CurrPos = LinkedListInsert(CurrPos, " goto DelayUSO" + Str(USDC))
				USDC += 1
			ElseIf Cycles >= 5 Then
				DS = Cycles \ 3
				IF DS > 255 THEN DS = 255
				Cycles -= (DS * 3 + 1) 'lose 1 cycle at loop end, but have 2 extra at start
				CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = " + Str(DS)) '2 cycles, movlw/movwf
				CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC))
				CurrPos = LinkedListInsert(CurrPos, " decfsz DELAYTEMP,F")
				CurrPos = LinkedListInsert(CurrPos, " goto DelayUS" + Str(USDC))
				USDC += 1
			ElseIf Cycles >= 2 AND ChipFamily <> 16 THEN
				CurrPos = LinkedListInsert(CurrPos, " goto $+1")
				Cycles -= 2
			ElseIf Cycles >= 1 THEN
				CurrPos = LinkedListInsert(CurrPos, " nop")
				Cycles -= 1
			END If
		
		ElseIf ModeAVR Then
			If Cycles >= 771 Then
				'Each 1 in DS wastes 3 + 3 * 256 = 771 us
				DS = Cycles \ 771
				If DS > 255 Then DS = 255
				Cycles -= (DS * 771)
				CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP2 = " + Str(DS))
				CurrPos = LinkedListInsert(CurrPos, "DelayUSO" + Str(USDC) + LabelEnd)
				CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = 0")
				CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC) + LabelEnd)
				CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP")
				CurrPos = LinkedListInsert(CurrPos, " brne DelayUS" + Str(USDC))
				CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP2")
				CurrPos = LinkedListInsert(CurrPos, " brne DelayUSO" + Str(USDC))
				USDC += 1
				
			ElseIf Cycles >= 6 Then
				'This delay can do multiples of 3 cycles
				'But for 3 cycles, it's more efficient to have the rjmp then the nop
				DS = Cycles \ 3
				IF DS > 255 THEN DS = 255
				Cycles -= (DS * 3) 'lose 1 cycle at loop end, but have 1 extra at start
				CurrPos = LinkedListInsert(CurrPos, "DELAYTEMP = " + Str(DS))
				CurrPos = LinkedListInsert(CurrPos, "DelayUS" + Str(USDC) + LabelEnd)
				CurrPos = LinkedListInsert(CurrPos, " dec DELAYTEMP")
				CurrPos = LinkedListInsert(CurrPos, " brne DelayUS" + Str(USDC))
				USDC += 1
			ElseIf Cycles >= 2 Then
				CurrPos = LinkedListInsert(CurrPos, " rjmp PC + 1") ' Was rjmp 0
				Cycles -= 2
			ElseIf Cycles >= 1 Then
				CurrPos = LinkedListInsert(CurrPos, " nop")
				Cycles -= 1
			End If
			
		Else
		
		End If
	Loop While Cycles > 0
	
	Return OutList
End Function

Function GenerateVectorCode As LinkedListElement Pointer
	
	Dim As Integer VectsAdded, CurrentVect, IntLoc, PD
	
	'Prepare output list
	Dim As LinkedListElement Pointer OutList, CurrLine
	Dim As SubType Pointer IntSub
	OutList = LinkedListCreate
	CurrLine = OutList
	
	CurrLine = LinkedListInsert(CurrLine, ";Vectors")
	'Generate PIC vectors
	If ModePIC Then
		'12 and 14 bit instruction width cores
		If ChipFamily = 12 Or ChipFamily = 14 Or ChipFamily = 15 Then
			If HasSFR("INTCON") Then
				CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader))
				CurrLine = LinkedListInsert(CurrLine, " goto BASPROGRAMSTART")
				CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader + 4))
				IntLoc = LocationOfSub("Interrupt", "")
				If IntLoc = 0 Then
					CurrLine = LinkedListInsert(CurrLine, " retfie")
				Else
					'Can't do this, must compile INTERRUPT inline
					'CurrLine = LinkedListInsert(CurrLine, " goto INTERRUPT")
					CurrLine = LinkedListInsert(CurrLine, "Interrupt")
					'Inline later
				End If
				CurrLine = LinkedListInsert(CurrLine, "")
				CurrLine = LinkedListInsert(CurrLine, Star80)
				CurrLine = LinkedListInsert(CurrLine, "")
			Else
				'Do nothing, page 0 will start immediately
			End If
			
		'16 bit instruction width cores
		ElseIf ChipFamily = 16 Then
			CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader))
			CurrLine = LinkedListInsert(CurrLine, " goto BASPROGRAMSTART")
			CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(Bootloader + 8))
			If LocationOfSub("Interrupt", "") = 0 Then
				CurrLine = LinkedListInsert(CurrLine, " retfie")
			Else
				CurrLine = LinkedListInsert(CurrLine, " goto INTERRUPT")
			End If
			CurrLine = LinkedListInsert(CurrLine, "")
			CurrLine = LinkedListInsert(CurrLine, Star80)
			CurrLine = LinkedListInsert(CurrLine, "")
		End If
	
	'AVR vectors
	ElseIf ModeAVR Then
		CurrLine = LinkedListInsert(CurrLine, ";Interrupt vectors")
		CurrLine = LinkedListInsert(CurrLine, ".ORG " + Str(Bootloader))
		CurrLine = LinkedListInsert(CurrLine, " rjmp BASPROGRAMSTART ;Reset")
			
		'Add ON ... GOSUB code
		VectsAdded = 0
		CurrentVect = 0
		ISRC = 0
		Do While VectsAdded < IntCount
			CurrentVect += 1
			'Print CurrentVect, IntCount, VectsAdded
			
			IntLoc = 0
			For PD = 1 to IntCount
				If Interrupts(PD).VectorLoc = CurrentVect Then IntLoc = PD: Exit For
			Next
			
			If IntLoc <> 0 Then
				With Interrupts(IntLoc)
					VectsAdded += 1
					CurrLine = LinkedListInsert(CurrLine, ".ORG " + Str(.VectorLoc + Bootloader))
					If .Handler = "" Then
						IF UserInt THEN
							CurrLine = LinkedListInsert(CurrLine, " rjmp INTERRUPT ;" + UCase(.Vector))
						Else
							CurrLine = LinkedListInsert(CurrLine, " reti ;" + UCase(.Vector))
						End If
					Else
						CurrLine = LinkedListInsert(CurrLine, " rjmp Int" + UCase(.Vector) + " ;" + UCase(.Vector))
						
					End If
				End With
			End If
		
		Loop
		
		CurrLine = LinkedListInsert(CurrLine, "")
		CurrLine = LinkedListInsert(CurrLine, Star80)
		CurrLine = LinkedListInsert(CurrLine, "")
		
	'Z8 vectors
	ElseIf ModeZ8 Then
		'Add ON ... GOSUB code
		For IntLoc = 1 To IntCount
			With Interrupts(IntLoc)
				If UCase(.Vector) = "RESET" Then
					CurrLine = LinkedListInsert(CurrLine, " VECTOR " + .Vector + " = BASPROGRAMSTART")
				Else
					If .Handler = "" Then
						IF UserInt THEN
							CurrLine = LinkedListInsert(CurrLine, " VECTOR " + .Vector + " = INTERRUPT")
						End If
					Else
						CurrLine = LinkedListInsert(CurrLine, " VECTOR " + .Vector + " = Int" + UCase(.Vector))
					End If
				End If
			End With
		Next
		
	End If
	
	Return OutList
End Function 

Function GetCalcType(VT1 As String, Act As String, VT2 As String) As String
	'Decide which type a calculation returns
	
	'Comparision operations return byte
	If Act = "=" Then Return "BYTE"
	If Act = "~" Then Return "BYTE"
	If Act = "<" Then Return "BYTE"
	If Act = ">" Then Return "BYTE"
	If Act = "{" Then Return "BYTE"
	If Act = "}" Then Return "BYTE"
	
	'Negate returns at least an integer
	If Act = "-" And VT1 = "" And CastOrder(VT2) < CastOrder("INTEGER") Then Return "INTEGER"
	
	'Other types return highest of two operands
	If CastOrder(VT1) > CastOrder(VT2) Then Return VT1
	Return VT2
	
End Function

Function GetCalcVar (VarTypeIn As String) As String
	'Get a calc var, and return its name
	Dim As String VarType, Temp
	Dim As Integer OutVar, HighReg, SV
	
	'Status can be A (available), U (used) or empty
	
	OutVar = -1
	HighReg = 0
	VarType = UCase(Trim(VarTypeIn))
	If Mid(VarType, 2, 1) = ":" Then
		Temp = Left(VarType, 1)
		VarType = Mid(VarType, 3)
		IF ModeAVR And Temp = "H" Then HighReg = -1
	End If
	
	'Find old, unused var
	'Aim to achieve highest of following:
	'= Type, = area
	'> Type, = area
	'* Type, = area
	'= Type, > area
	'>= Type, > area
	'* Type, > area
	'If still none, make new
	
	'Correct type, correct area
	For SV = 1 to TCVC
		With CalcVars(SV)
			If .MaxType = VarType And .Status = "A" And HighReg = .High Then
				OutVar = SV
				Exit For
			End If
		End With
	Next
	
	'Any type that is bigger, correct area
	If OutVar = -1 Then
		For SV = 1 to TCVC
			With CalcVars(SV)
				If CastOrder(.MaxType) > CastOrder(VarType) And .Status = "A" And HighReg = .High Then
					OutVar = SV
					Exit For
				End If
			End With
		Next
	End If
	
	'Any type, correct area
	If OutVar = -1 Then
		For SV = 1 to TCVC
			With CalcVars(SV)
				If .Status = "A" And HighReg = .High Then
					OutVar = SV
					.MaxType = VarType 'Increase type
					Exit For
				End If
			End With
		Next
	End If
	
	'Correct type, higher area
	If Not HighReg Then
		If OutVar = -1 Then
			For SV = 1 to TCVC
				With CalcVars(SV)
					If .MaxType = VarType And .Status = "A" Then
						OutVar = SV
						Exit For
					End If
				End With
			Next
		End If
		
		'Any type that is bigger, higher area
		If OutVar = -1 Then
			For SV = 1 to TCVC
				With CalcVars(SV)
					If CastOrder(.MaxType) > CastOrder(VarType) And .Status = "A" Then
						OutVar = SV
						Exit For
					End If
				End With
			Next
		End If
		
		'Any type, higher area
		If OutVar = -1 Then
			For SV = 1 to TCVC
				With CalcVars(SV)
					If .Status = "A" Then
						OutVar = SV
						.MaxType = VarType 'Increase type
						Exit For
					End If
				End WIth
			Next
		End If
	End If
		
	'If no unused vars, make new one
	If OutVar = -1 Then
		TCVC += 1
		OutVar = TCVC
		With CalcVars(OutVar)
			.MaxType = VarType
			.High = 0
		End With
	End If
	
	'Mark var as used, set type and return name
	With CalcVars(OutVar)
		.Status = "U" 'Mark used
		.CurrentType = VarType 'Set type
		.CurrentValue = "" 'Clear value
		If HighReg Then
			.High = -1
		End If
	End With
	Return "SysTemp" + Str(OutVar)
	
End Function

Function GetCalledSubs(CurrSub As SubType Pointer, ExistingList As LinkedListElement Pointer) As LinkedListElement Pointer
	'Gets a list of all subs called from CurrSub
	'Will also return subs called by subs
	
	Dim As LinkedListElement Pointer OutList, CurrPos, SearchList, CurrCall
	Dim As Integer CallFound
	
	If ExistingList = 0 Then
		OutList = LinkedListCreate
	Else
		OutList = ExistingList
	End If
	CurrPos = OutList
	Do While CurrPos->Next <> 0
		CurrPos = CurrPos->Next
	Loop
	
	With *CurrSub
		CurrCall = .CallList->Next
		Do While CurrCall <> 0
			
			'Check to see if call is already in output list
			SearchList = OutList->Next
			CallFound = 0
			Do While SearchList <> 0
				If SearchList->MetaData = CurrCall->MetaData Then
					CallFound = -1
					Exit Do
				End If
				SearchList = SearchList->Next
			Loop
			If CallFound = 0 Then
				CurrPos = LinkedListInsert(CurrPos, CurrCall->MetaData)
				GetCalledSubs(CurrCall->MetaData, OutList)
				Do While CurrPos->Next <> 0
					CurrPos = CurrPos->Next
				Loop
			End If
			
			CurrCall = CurrCall->Next
		Loop
	End With
	
	Return OutList
End Function

FUNCTION GetDestSub(Origin As String) As Integer
	Dim As String Temp
	
	If Origin = "ALL" OR Origin = "" Then Return 0
	If InStr(Origin, "D") = 0 Then Return GetSubID(Origin)
	Return VAL(Trim(Mid(Origin, INSTR(Origin, "D") + 1)))
	
END FUNCTION

Sub GetEqConfig
	'Generate a list of all config names/settings that are equivalent
	
	Dim As LinkedListElement Pointer CurrLoc, EqSettingsLoc
	
	If ModePIC Then
		'EqConfigSettings is a linked list of linked lists
		'Each sub list in the list contains all equivalent settings
		EqConfigSettings = LinkedListCreate
		EqSettingsLoc = EqConfigSettings
		
		'MCLR/MCLRE
		CurrLoc = LinkedListCreate
		EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
		CurrLoc = LinkedListInsert(CurrLoc, "MCLR")
		CurrLoc = LinkedListInsert(CurrLoc, "MCLRE")
		
		'OSC/FOSC/EXTRC/INTRC/INTIO
		CurrLoc = LinkedListCreate
		EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
		CurrLoc = LinkedListInsert(CurrLoc, "OSC")
		CurrLoc = LinkedListInsert(CurrLoc, "FOSC")
		CurrLoc = LinkedListInsert(CurrLoc, "EXTRC")
		CurrLoc = LinkedListInsert(CurrLoc, "INTRC")
		CurrLoc = LinkedListInsert(CurrLoc, "INTIO")
		
		'BODEN/BOREN/BOD
		CurrLoc = LinkedListCreate
		EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
		CurrLoc = LinkedListInsert(CurrLoc, "BODEN")
		CurrLoc = LinkedListInsert(CurrLoc, "BOREN")
		CurrLoc = LinkedListInsert(CurrLoc, "BOD")
		
		'WDT/WDTEN
		CurrLoc = LinkedListCreate
		EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
		CurrLoc = LinkedListInsert(CurrLoc, "WDT")
		CurrLoc = LinkedListInsert(CurrLoc, "WDTE")
		CurrLoc = LinkedListInsert(CurrLoc, "WDTEN")
		
		'PWRT/PWRTE
		CurrLoc = LinkedListCreate
		EqSettingsLoc = LinkedListInsert(EqSettingsLoc, CurrLoc)
		CurrLoc = LinkedListInsert(CurrLoc, "PWRT")
		CurrLoc = LinkedListInsert(CurrLoc, "PWRTE")
		
	End If
	
End Sub

Function GetLabelList(CompSub As SubType Pointer) As LinkedListElement Pointer
	
	'Search a subroutine, return a list of all labels in it
	
	Dim As LinkedListElement Pointer LabelList, LabelListPos, CurrLine
	
	LabelList = LinkedListCreate
	LabelListPos = LabelList
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		If IsASM(CurrLine->Value, 1) = 0 Then
			If LabelEnd <> "" Then
				If Right(CurrLine->Value, 1) = LabelEnd Then
					LabelListPos = LinkedListInsert(LabelListPos, UCase(Left(CurrLine->Value, Len(CurrLine->Value) - 1)))
					LabelListPos->MetaData = CurrLine
				End If
			Else
				If CurrLine->Value <> "" And Left(CurrLine->Value, 1) <> ";" And Left(CurrLine->Value, 1) <> " " And Left(CurrLine->Value, 9) <> "PRESERVE " Then
					LabelListPos = LinkedListInsert(LabelListPos, UCase(CurrLine->Value))
					LabelListPos->MetaData = CurrLine
				End If
			End If
		End If
		CurrLine = CurrLine->Next
	Loop
	
	Return LabelList
End Function

Function GetMetaData(CurrLine As LinkedListElement Pointer) As ProgLineMeta Pointer
	'Get the ProgLineMeta object for the current line
	'If none exists, create one
	
	'If CurrLine is null pointer, return null pointer
	If CurrLine = 0 Then Return 0
	
	'Check for metadata, create if needed
	If CurrLine->MetaData = 0 Then
		CurrLine->MetaData = NewProgLineMeta
	End If
	
	Return CurrLine->MetaData
	
End Function

Function GetPinDirection(PinNameIn As String) As PinDirType Pointer
	'Get the PinDirections list element metadata for the pin
	'Will add the pin to the list if no data found for it
	
	Dim As String PinName, CheckPin, PinDirVar
	Dim As LinkedListElement Pointer CurrItem, PinListItem
	Dim As PinDirType Pointer PinDirItem
	
	PinName = UCase(Trim(PinNameIn))
	
	'Check that pin is a valid I/O pin
	'Return 0 if it isn't
	'Should be in form {PORT | GPIO | LAT | PIN}[A|B|C|D|E]
	'Can also accept an entire port
	If InStr(PinName, ".") = 0 Then
		CheckPin = PinName
	Else
		CheckPin = Left(PinName, InStr(PinName, ".") - 1)
	End If
	
	'If CheckPin is not PORTx Or PORTxx,
	If Left(CheckPin, 4) <> "PORT" Or (Len(CheckPin) <> 5 And Len(CheckPin) <> 6) Then
		'and not GPIO,
		If CheckPin <> "GPIO" Then
			'and not LATx,
			If Left(CheckPin, 3) <> "LAT" Or Len(CheckPin) <> 4 Then
				'and not PINx,
				If Left(CheckPin, 3) <> "PIN" Or Len(CheckPin) <> 4 Then
					'then CheckPin is not a valid pin
					Return 0
				End If
			End If
		End If
	End If
	'Not IO, not a pin
	If Not IsIOReg(CheckPin) Then Return 0
	
	'Check that direction of pin can be set
	'Get the variable that sets port direction
	PinDirVar = CheckPin
	Replace PinDirVar, "PORT", ""
	Replace PinDirVar, "GP", ""
	Replace PinDirVar, "LAT", ""
	Replace PinDirVar, "PIN", ""
	If ModePIC Then
		PinDirVar = "TRIS" + PinDirVar
	ElseIf ModeAVR Then
		PinDirVar = "DDR" + PinDirVar
	End If
	'TRISIO will not be IO on 12 bit PIC, but will be used as direction buffer
	If ChipFamily <> 12 And Not IsIOReg(PinDirVar) Then Return 0
	
	'If pin name starts with LAT or PIN, replace with PORT
	If Left(PinName, 3) = "LAT" Then PinName = "PORT" + Mid(PinName, 4)
	If Left(PinName, 3) = "PIN" Then PinName = "PORT" + Mid(PinName, 4)
	
	'Check for pin in existing pin direction list
	PinListItem = 0
	CurrItem = PinDirections->Next
	Do While CurrItem <> 0
		If CurrItem->Value = PinName Then
			PinListItem = CurrItem
		End If
		CurrItem = CurrItem->Next
	Loop
	
	'If pin isn't in list, add it
	If PinListItem = 0 Then
		PinListItem = LinkedListInsert(PinDirections, PinName)
	End If
	
	'Get pin direction data
	PinDirItem = PinListItem->MetaData
	'If no direction data exists, create it
	If PinDirItem = 0 Then
		PinDirItem = Callocate(SizeOf(PinDirType))
		PinListItem->MetaData = PinDirItem
	End If
	
	'Return direction data
	Return PinDirItem
	
End Function

Function GetRealIOName(InName As String) As String
	'Used on AVR to improve compatibility with existing GCBASIC programs
	'Typically called when generating pin reading code
	'Changes PORTx to PINx, and LATx to PORTx, making AVR and PIC18 compatible
	
	'AVR only
	If Not ModeAVR Then Return InName
	
	Dim As String OutName
	'Do replacements
	If Left(UCase(InName), 4) = "PORT" And Len(InName) = 5 Then
		OutName = "PIN" + Mid(InName, 5)
	ElseIf Left(UCase(InName), 3) = "LAT" And Len(InName) = 4 Then
		OutName = "PORT" + Mid(InName, 4)
	Else
		OutName = InName
	End If
	
	Return OutName
End Function

Function GetRegisterLoc(RegName As String) As Integer
	
	'Returns the location for a system variable
	'Returns -1 if no location found
	
	Dim RegNameTidy As String
	Dim As Integer DestLoc, CurrBank
	DestLoc = -1
	
	If ModePIC Then
		'Get the location for the register
		RegNameTidy = LCase(Trim(RegName))
		Select Case RegNameTidy
			
			Case "syscalctempx", "sysbytetempx", "syswordtempx", "sysintegertempx", "syslongtempx", "delaytemp": DestLoc = 0
			Case "syscalctempx_h", "syswordtempx_h", "sysintegertempx_h", "syslongtempx_h", "delaytemp2": DestLoc = 1
			Case "syscalctempx_u", "syslongtempx_u", "sysdivmultx", "syswaittempms", "sysstringb": DestLoc = 2
			Case "syscalctempx_e", "syslongtempx_e", "sysdivmultx_h", "syswaittempms_h", "sysstringb_h": DestLoc = 3
			Case "sysdivloop", "syswaittemp10ms", "syswaittemps": DestLoc = 4
			Case "syscalctempa", "sysbytetempa", "syswordtempa", "sysintegertempa", "syslongtempa": DestLoc = 5
			Case "syswaittempus", "syswaittemp10us", "syswaittempm": DestLoc = 5
			Case "syscalctempa_h", "syswordtempa_h", "sysintegertempa_h", "syslongtempa_h": DestLoc = 6
			Case "sysstringlength", "syswaittempus_h", "syswaittemph": DestLoc = 6
			Case "syscalctempa_u", "syslongtempa_u", "sysdivmulta", "sysstringa": DestLoc = 7
			Case "syscalctempa_e", "syslongtempa_e", "sysdivmulta_h", "sysstringa_h": DestLoc = 8	
			Case "syscalctempb", "sysbytetempb", "syswordtempb", "sysintegertempb", "syslongtempb": DestLoc = 9
			Case "syscalctempb_h", "syswordtempb_h", "sysintegertempb_h", "syslongtempb_h": DestLoc = 10
			Case "syscalctempb_u", "syslongtempb_u", "sysdivmultb", "sysreada": DestLoc = 11
			Case "syscalctempb_e", "syslongtempb_e", "sysdivmultb_h", "sysreada_h": DestLoc = 12
			Case "syssignbyte": DestLoc = 13
			Case "sysw": DestLoc = 14
			Case "sysstatus": DestLoc = 15
		End Select
		
		'Check that DestLoc actually exists
		If DestLoc <> -1 Then
			For CurrBank = 1 To NoBankLocs
				With NoBankLoc(CurrBank)
					'If the location is in a shared bank, return location
					If .StartLoc + DestLoc <= .EndLoc Then
						Return DestLoc
					End If
				End With
			Next
			DestLoc = -1
		End If
		
	ElseIf ModeAVR Then
		'Get the location for the register
		RegNameTidy = LCase(Trim(RegName))
		Select Case RegNameTidy
			Case "syscalctempx", "sysbytetempx", "syswordtempx", "sysintegertempx", "syslongtempx": DestLoc = 0
			Case "syscalctempx_h", "syswordtempx_h", "sysintegertempx_h", "syslongtempx_h": DestLoc = 1
			Case "syscalctempx_u", "syslongtempx_u", "sysdivmultx": DestLoc = 2
			Case "syscalctempx_e", "syslongtempx_e", "sysdivmultx_h": DestLoc = 3
			Case "syssignbyte": DestLoc = 4
			Case "sysdivloop": DestLoc = 5
			
			Case "sysvaluecopy": DestLoc = 21
			Case "syscalctempa", "sysbytetempa", "syswordtempa", "sysintegertempa", "syslongtempa": DestLoc = 22
			Case "syscalctempa_h", "syswordtempa_h", "sysintegertempa_h", "syslongtempa_h": DestLoc = 23
			Case "syscalctempa_u", "syslongtempa_u", "sysdivmulta": DestLoc = 24
			Case "delaytemp", "syscalctempa_e", "syslongtempa_e", "sysdivmulta_h", "sysstringlength": DestLoc = 25
			Case "delaytemp2", "sysstringa": DestLoc = 26
			Case "syswaittempus", "syswaittemp10us", "syswaittempm": DestLoc = 27
			Case "sysstringa_h": DestLoc = 27
			Case "syswaittempus_h", "syswaittemph", "syscalctempb", "sysbytetempb", "syswordtempb", "sysintegertempb", "syslongtempb", "sysstringb": DestLoc = 28
			Case "syswaittempms", "syscalctempb_h", "syswordtempb_h", "sysintegertempb_h", "syslongtempb_h", "sysstringb_h": DestLoc = 29
			Case "syswaittempms_h", "syscalctempb_u", "syslongtempb_u", "sysdivmultb", "sysreada": DestLoc = 30
			Case "syswaittemp10ms", "syswaittemps", "syscalctempb_e", "syslongtempb_e": DestLoc = 31
			Case "sysdivmultb_h", "sysreada_h": DestLoc = 31
			
		End Select
	End If
	
	Return DestLoc
	
End Function

FUNCTION GetSub(Origin As String) As String
	Dim As String Temp
	Dim As Integer T
	
	IF Origin = "ALL" OR Origin = "" Then Return "ALL"
	Temp = UCase(Trim(Mid(Origin, INSTR(Origin, "S") + 1)))
	IF INSTR(Temp, "D") <> 0 THEN Temp = Left(Temp, INSTR(Temp, "D") - 1)
	T = VAL(Temp)
	IF T = 0 THEN Return "ALL"
	
	Temp = Trim(Subroutine(T)->Name)
	IF INSTR(Temp, "(") <> 0 Then Temp = Left(Temp, INSTR(Temp, "(") - 1)
	IF INSTR(Temp, " ") <> 0 Then Temp = Left(Temp, INSTR(Temp, " ") - 1)
	IF Temp = "" THEN Temp = "ALL"
	GetSub = Trim(UCase(Temp))
END Function

Function GetSubFullName(SubIndex As Integer) As String
	
	Dim As String SubNameOut
	Dim As SubType Pointer CompSub
	CompSub = Subroutine(SubIndex)
	
	'Get the name that the sub will receive when written to the asm
	If CompSub->Overloaded Then
		SubNameOut = CompSub->Name + Str(SubIndex)
	Else
		SubNameOut = CompSub->Name
	End If
	If CompSub->IsFunction Then
		SubNameOut = "FN_" + SubNameOut
	End If
	
	Return SubNameOut
End Function

FUNCTION GetSubID(Origin As String) As Integer
	Dim As String Temp
	
	If Origin = "ALL" OR Origin = "" Then Return 0
	Temp = UCase(Trim(Mid(Origin, INSTR(Origin, "S") + 1)))
	IF INSTR(Temp, "D") <> 0 THEN Temp = Left(Temp, INSTR(Temp, "D") - 1)
	Return VAL(Temp)
	
END Function

Function GetSubSig(CurrentSub As SubType) As String
	Dim As String OutData, NewData
	Dim As Integer PD
	
	OutData = ""
	With CurrentSub
		For PD = 1 to .ParamCount
			With .Params(PD)
				NewData = ""
				Select Case UCASE(.Type)
					Case "BIT", "BYTE", "WORD", "INTEGER", "LONG", "FLOAT", "STRING": NewData = UCase(.Type) + ":"
				End Select
				If .Default <> "" Then NewData = LCase(NewData)
				OutData += NewData
			End With
		Next
	End With
	
	Return OutData
End Function

Function GetSubParam (ParamIn As String, ForceIn As Integer) As SubParam
	'Parse a text representation of a single sub parameter, and output a
	'matching SubParam item
	'If ForceIn = -1, parameter will be treated as input
	
	Dim ParamOut As SubParam
	Dim As String TempData, ParamData
	
	ParamData = UCase(Trim(ParamIn))
	
	With ParamOut
		.Dir = 3
		.Default = ""
		
		'In/Out, OPTIONAL
		CheckStartAgain:
		'Print ParamData
		If Left(ParamData, 3) = "IN " Then
			.Dir = 1
			ParamData = Mid(ParamData, 4)
			Goto CheckStartAgain
		ElseIf Left(ParamData, 4) = "OUT " Then
			.Dir = 2
			ParamData = Mid(ParamData, 5)
			Goto CheckStartAgain
		ElseIf Left(ParamData, 9) = "OPTIONAL " Then
			.Default = Trim(Mid(ParamData, Instr(ParamData, "=") + 1))
			ParamData = Trim(Left(ParamData, Instr(ParamData, "=") - 1))
			ParamData = Trim(Mid(ParamData, 10))
			Goto CheckStartAgain
		'Optional is optional, might just have the default value
		ElseIf InStr(ParamData, "=") <> 0 Then
			.Default = Trim(Mid(ParamData, Instr(ParamData, "=") + 1))
			ParamData = Trim(Left(ParamData, Instr(ParamData, "=") - 1))
			Goto CheckStartAgain
		End If
		
		'Type
		If INSTR(ParamData, " AS ") <> 0 Then
			.Type = Mid(ParamData, Instr(ParamData, " AS ") + 4)
			ParamData = Left(ParamData, Instr(ParamData, " AS ") - 1)
		Else
			.Type = "BYTE"
		End If
		
		'Name
		.Name = Trim(ParamData)
		
		'Type detection for () and $
		If Right(.Name, 2) = "()" Then
			.Name = Left(.Name, Len(.Name) - 2)
			.Type = .Type + "()"
		ElseIf Right(.Name, 1) = "$" Then
			.Name = Left(.Name, Len(.Name) - 1)
			.Type = "STRING"
		End If
		
		'Force input?
		If ForceIn Then
			.Dir = 1
		End If
		
	End With
	
	Return ParamOut
	
End Function

SUB InitCompiler
	
	'Misc temp vars
	Dim As String Temp, DataSource, MessagesFile, LangMessagesFile, LangName, MsgName, MsgVal
	Dim As String SettingsFile(20), CurrentTag, ParamUpper, LeftThree, NewFileName
	Dim As Integer T, Block, CD, FCB, IniNotSet, SettingsFileMode, FindTool, FindMsg
	Dim As Integer SettingsFiles, CurrSettingsFile
	
	Dim As Integer AsmNotSet, ProgNotSet, OutNotSet, PresNotSet, VbsNotSet
	Dim As Integer PauseNotSet, ReportNotSet
	
	'Detect GCBASIC install directory
	Temp = COMMAND(0)
	For T = LEN(Temp) to 1 Step -1
		If Mid(Temp, T, 1) = "\" Or Mid(Temp, T, 1) = "/" Then ID = Left(Temp, T - 1): Exit For
	Next
	If ID = "" Or ID = "." THEN
		ID = CURDIR
		#IFDEF __FB_LINUX__
			If Right(ID, 1) = "/" Then ID = Left(ID, Len(ID) - 1)
		#ELSE
			If Right(ID, 1) = "\" Then ID = Left(ID, Len(ID) - 1)
		#ENDIF
	End If
	
	'Clear parameters
	OFI = ""
	AsmExe = ""
	PrgExe = ""
	AsmTool = 0
	PrgTool = 0
	VBS = 0
	PauseOnErr = 1
	PauseAfterCompile = 0
	GCGB = 0
	CompReportFormat = "html"
	
	SettingsFile(1) = "gcbasic.ini"
	SettingsFiles = 1
	AsmNotSet = -1
	ProgNotSet = -1
	OutNotSet = -1
	PresNotSet = -1
	VbsNotSet = -1
	PauseNotSet = -1
	IniNotSet = -1
	ReportNotSet = -1
	
	'Read parameters
	CD = 1
	Do While COMMAND(CD) <> ""
		DataSource = COMMAND(CD)
		ParamUpper = Ucase(COMMAND(CD))
		LeftThree = Left(ParamUpper, 3)
		
		'Simple options
		'Verbose mode
		If ParamUpper = "/V" Or ParamUpper = "-V" Then
			VBS = 1
			VbsNotSet = 0
			
		'Show licence
		ElseIf ParamUpper = "/L" Or ParamUpper = "-L" Then
			ShowBlock "License"
			END
			
		'Pause on errors?
		ElseIf ParamUpper = "/NP" Or ParamUpper = "-NP" Then
			PauseOnErr = 0
			PauseNotSet = 0
			
		'Great Cow Graphical BASIC Mode?
		'(Alters the error listing format)
		ElseIf ParamUpper = "/GCGB" Then
			GCGB = 1
			
		'Complex options
		'Settings file
		ElseIf LeftThree = "/S:" Or LeftThree = "-S:" Then
			SettingsFile(1) = Mid(DataSource, 4)
			IniNotSet = 0
		
		'Assembler command
		ElseIf LeftThree = "/A:" Or LeftThree = "-A:" Then
			AsmExe = Trim(Mid(DataSource, 4))
			AsmParams = ""
			If Left(AsmExe, 1) = Chr(34) Then
				For FCB = 2 to Len(AsmExe)
					If Mid(AsmExe, FCB, 1) = Chr(34) Then
						AsmParams = Trim(Mid(AsmExe, FCB + 1, 1))
						AsmExe = Trim(Left(AsmExe, FCB))
					End If
				Next
			ElseIf Instr(AsmExe, " ") <> 0 Then
				AsmParams = Mid(AsmExe, Instr(AsmExe, " ") + 1)
				AsmExe = Left(AsmExe, Instr(AsmExe, " ") - 1)
			End If
			
			AsmNotSet = 0
			'Do While INSTR(MakeASM, Chr(34)) <> 0: Replace MakeASM, Chr(34), "": Loop
			
		'Programmer command
		ElseIf LeftThree = "/P:" Or LeftThree = "-P:" Then
			PrgExe = Trim(Mid(DataSource, 4))
			PrgParams = ""
			PrgDir = ""
			If Left(PrgExe, 1) = Chr(34) Then
				For FCB = 2 to Len(PrgExe)
					If Mid(PrgExe, FCB, 1) = Chr(34) Then
						PrgParams = Trim(Mid(PrgExe, FCB + 1, 1))
						PrgExe = Trim(Left(PrgExe, FCB))
					End If
				Next
			ElseIf Instr(PrgExe, " ") <> 0 Then
				PrgParams = Mid(PrgExe, Instr(PrgExe, " ") + 1)
				PrgExe = Left(PrgExe, Instr(PrgExe, " ") - 1)
			End If
			ProgNotSet = 0
			'Do While INSTR(SendToPIC, Chr(34)) <> 0: Replace SendToPIC, Chr(34), "": Loop
			
		'Output filename
		ElseIf LeftThree = "/O:" Or LeftThree = "-O:" Then
			OFI = Mid(DataSource, 4)
			OutNotSet = 0
			'Do While INSTR(OFI, Chr(34)) <> 0: Replace OFI, Chr(34), "": Loop
			
		'Preserve mode
		ElseIf LeftThree = "/K:" Or LeftThree = "-K:" Then
			Temp = Right(ParamUpper, 1)
			IF Temp = "C" OR Temp = "1" Then PreserveMode = 1
			IF Temp = "A" OR Temp = "2" Then PreserveMode = 2
			If Temp = "L" OR Temp = "3" Then PreserveMode = 3
			PresNotSet = 0
			
		'Report mode
		ElseIf LeftThree = "/R:" Or LeftThree = "-R:" Then
			CompReportFormat = LCase(Mid(DataSource, 4))
			ReportNotSet = 0
			
		'Deprecated options
		'Clear screen
		ElseIf ParamUpper = "/NC" or ParamUpper = "-NC" Then
			'CLS
			
		'Compiler directory
		ElseIf LeftThree = "/D:" Or LeftThree = "-D:" Then
			
		'Input filename
		Else
			FI = ShortName(DataSource)
			
		End If
		
		CD += 1
	Loop
	
	'Read settings file
	CurrSettingsFile = 0
	Do
		CurrSettingsFile += 1
		
		#IFDEF __FB_LINUX__
			If Instr(SettingsFile(CurrSettingsFile), "/") = 0 Then SettingsFile(CurrSettingsFile) = ID + "/" + SettingsFile(CurrSettingsFile)
		#ELSE
			If Instr(SettingsFile(CurrSettingsFile), "\") = 0 Then SettingsFile(CurrSettingsFile) = ID + "\" + SettingsFile(CurrSettingsFile)
		#ENDIF
		If Dir(SettingsFile(CurrSettingsFile)) = "" Then
			If VBS = 1 And IniNotSet = 0 Then Print "Cannot find settings file " + SettingsFile(CurrSettingsFile)
		Else
			Open SettingsFile(CurrSettingsFile) For Input As #1
			'Decide mode to read file in
			SettingsFileMode = 0
			Do While Not Eof(1)
				Line Input #1, DataSource
				DataSource = Trim(DataSource)
				If Left(DataSource, 1) = "[" And Right (DataSource, 1) = "]" Then
					SettingsFileMode = 1
					Exit Do
				End If
			Loop
			Seek #1, 1
			'Read actual file
			CurrentTag = ""
			If SettingsFileMode = 0 Then CurrentTag = "gcbasic"
			Do While Not EOF(1)
				Line Input #1, DataSource
				Do While InStr(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": Loop
				DataSource = Trim(DataSource)
				
				If DataSource <> "" And Left(DataSource, 1) <> "'" Then
					
					If Left(DataSource, 8) = "include " Then
						NewFileName = Trim(Mid(DataSource, 9))
						
						'Replace %APPDATA% with APPDATA environment var
						NewFileName = ReplaceToolVariables(NewFileName)
												
						Dim As Integer CheckFile, FileFound
						FileFound = 0
						For CheckFile = 1 To SettingsFiles
							If SettingsFile(CheckFile) = NewFileName Then FileFound = -1: Exit For
						Next
						If Not FileFound Then
							SettingsFiles += 1
							SettingsFile(SettingsFiles) = NewFileName
						End If
						
					ElseIf SettingsFileMode = 1 And Left(DataSource, 1) = "[" And Right(DataSource, 1) = "]" Then
						CurrentTag = LCase(Mid(DataSource, 2, Len(DataSource) - 2))
						
						If Left(CurrentTag, 4) = "tool" Then
							ToolCount += 1
							With Tool(ToolCount)
								If InStr(CurrentTag, "=") <> 0 Then
									.Name = LCase(Trim(Mid(CurrentTag, InStr(CurrentTag, "=") + 1)))
								Else
									.Name = ""
								End If
								.Type = ""
								.Cmd = ""
								.Params = ""
								.ExtraParams = 0
							End With
						EndIf
						
					Else
						MsgName = LCase(Trim(Left(DataSource, INSTR(DataSource, "=") - 1)))
						MsgVal = Trim(Mid(DataSource, INSTR(DataSource, "=") + 1))
						
						If CurrentTag = "gcbasic" Then
							Select Case MsgName
								Case "assembler"
								If AsmNotSet Then AsmExe = MsgVal
								
								Case "assemblerparams"
								If AsmNotSet Then AsmParams = MsgVal
								
								Case "programmer"
								If ProgNotSet Then PrgExe = MsgVal
								
								Case "programmerparams"
								If ProgNotSet Then PrgParams = MsgVal
								
								Case "output"
								If OutNotSet Then OFI = MsgVal
								
								Case "preserve"
								If PresNotSet Then
									Select Case LCase(Left(MsgVal, 1))
										Case "c", "1": PreserveMode = 1
										Case "a", "2": PreserveMode = 2
										Case "l", "3": PreserveMode = 3
									End Select
								End If
								
								Case "workingdir"
								If LCase(MsgVal) = "tempdir" Or LCase(MsgVal) = "instdir" Then
									MsgVal = "%" + MsgVal + "%"
								End If 
								MsgVal = ReplaceToolVariables(MsgVal)
								ChDir MsgVal
								
								Case "verbose"
								If VbsNotSet Then
									Select Case LCase(Left(MsgVal, 1))
										Case "y", "t", "1": VBS = 1
										Case "n", "f", "0": VBS = 0
									End Select
								End If
								
								Case "pauseonerror"
								If PauseNotSet Then
									Select Case LCase(Left(MsgVal, 1))
										Case "y", "t", "1": PauseOnErr = 1
										Case "n", "f", "0": PauseOnErr = 0
									End Select
								End If
								
								Case "pauseaftercompile"
									Select Case LCase(Left(MsgVal, 1))
										Case "y", "t", "1": PauseAfterCompile = -1
										Case "n", "f", "0": PauseAfterCompile = 0
									End Select
								
								Case "language"
									LangName = MsgVal
									
								Case "report"
									If ReportNotSet Then
										CompReportFormat = LCase(MsgVal)
									End If
								
							End Select
							
						ElseIf Left(CurrentTag, 4) = "tool" Then
							Select Case MsgName
								Case "name"
									Tool(ToolCount).Name = LCase(MsgVal)
								Case "type"
									Tool(ToolCount).Type = LCase(MsgVal)
								Case "command"
									Tool(ToolCount).Cmd = MsgVal
								Case "params"
									Tool(ToolCount).Params = MsgVal
								Case "workingdir"
									Tool(ToolCount).WorkingDir = MsgVal
								Case Else
									With Tool(ToolCount)
										If .ExtraParams < 5 Then
											.ExtraParams += 1
											.ExtraParam(.ExtraParams, 1) = MsgName
											.ExtraParam(.ExtraParams, 2) = MsgVal
										End If
									End With
							End Select
						
						End If
					End If
				End If
			Loop
			Close
		End If
	Loop While CurrSettingsFile < SettingsFiles
	
	'Trim quotes from exe names
	AsmExe = Trim(AsmExe, Chr(34))
	PrgExe = Trim(PrgExe, Chr(34))
	
	'If tool specified for assembler or programmer, use it
	For FindTool = 1 To ToolCount
		With Tool(FindTool)
			If .Name = LCase(AsmExe) Then
				AsmExe = ReplaceToolVariables(.Cmd, , , @Tool(FindTool))
				AsmParams = .Params
				AsmTool = @Tool(FindTool)
			ElseIf .Name = LCase(PrgExe) Then
				PrgExe = ReplaceToolVariables(.Cmd, , , @Tool(FindTool))
				PrgParams = .Params
				PrgDir = .WorkingDir
				PrgTool = @Tool(FindTool)
			End If
		End With
	Next
	
	'Add full path to assembler and programmer names
	#IFDEF __FB_LINUX__
		If AsmExe <> "" And Left(AsmExe, 1) = "." Then AsmExe = ID + Mid(AsmExe, 2)
		If PrgExe <> "" And Left(PrgExe, 1) = "." Then PrgExe = ID + Mid(PrgExe, 2)
	#ELSE
		If AsmExe <> "" And Mid(AsmExe, 2, 1) <> ":" And UCASE(AsmExe) <> "GCASM" Then
			If Left(AsmExe, 1) = "\" Then AsmExe = Mid(AsmExe, 2)
			AsmExe = ID + "\" + AsmExe
		End If
		If PrgExe <> "" And Mid(PrgExe, 2, 1) <> ":" Then
			If Left(PrgExe, 1) = "\" Then PrgExe = Mid(PrgExe, 2)
			PrgExe = ID + "\" + PrgExe
		End If
	#ENDIF
	
	'Read message list
	#IFDEF __FB_LINUX__
		MessagesFile = ID + "/messages.dat"
		LangMessagesFile = ID + "/messages-" + LCase(LangName) + ".dat"
	#ELSE
		MessagesFile = ID + "\messages.dat"
		LangMessagesFile = ID + "/messages-" + LCase(LangName) + ".dat"
	#ENDIF
	IF Dir(MessagesFile) = "" THEN
		PRINT "Cannot find " + MessagesFile + "! Great Cow BASIC cannot operate"
		PRINT "without this file."
		If PauseOnErr = 1 THEN
			PRINT
			PRINT "Press any key to continue"
			DO WHILE INKEY = "": SLEEP 10: LOOP
		END IF
		END
	END IF
	
	'Load localised messages file
	If LangName <> "" Then
		If Dir(LangMessagesFile) = "" Then
			Print "Could not find localised message file " + LangMessagesFile
		Else
			OPEN LangMessagesFile FOR INPUT AS #1
			Block = 0
			DO WHILE NOT EOF(1)
				LINE INPUT #1, DataSource
				DataSource = Trim(DataSource)
				IF Left(UCase(DataSource), 6) = "BLOCK " THEN
					Block = 1
				ElseIF Left(UCase(DataSource), 9) = "END BLOCK" THEN
					Block = 0
				ElseIF Left(DataSource, 1) = "'" THEN
					DataSource = ""
				ElseIF INSTR(DataSource, "=") <> 0 THEN
					MsgName = Left(DataSource, INSTR(DataSource, "=") - 1)
					MsgVal = Mid(DataSource, INSTR(DataSource, "=") + 1)
					IF INSTR(MsgVal, Chr(34)) <> 0 THEN
						MsgVal = Mid(MsgVal, INSTR(MsgVal, Chr(34)) + 1)
						MsgVal = Left(MsgVal, INSTR(MsgVal, Chr(34)) - 1)
					END IF
					'Add message if it doesn't exist already
					MsgName = Trim(UCase(MsgName))
					T = 0
					For FindMsg = 1 To MSGC
						If MsgName = Messages(1, FindMsg) Then T = FindMsg: Exit For
					Next
					If T = 0 Then
						MSGC = MSGC + 1
						Messages(1, MSGC) = MsgName
						Messages(2, MSGC) = MsgVal
					End If
				END IF
			LOOP
			CLOSE #1
		End If
	End If
	
	OPEN MessagesFile FOR INPUT AS #1
	Block = 0
	DO WHILE NOT EOF(1)
		LINE INPUT #1, DataSource
		DataSource = Trim(DataSource)
		IF Left(UCase(DataSource), 6) = "BLOCK " THEN
			Block = 1
		ElseIF Left(UCase(DataSource), 9) = "END BLOCK" THEN
			Block = 0
		ElseIF Left(DataSource, 1) = "'" THEN
			DataSource = ""
		ElseIF INSTR(DataSource, "=") <> 0 THEN
			MsgName = Left(DataSource, INSTR(DataSource, "=") - 1)
			MsgVal = Mid(DataSource, INSTR(DataSource, "=") + 1)
			IF INSTR(MsgVal, Chr(34)) <> 0 THEN
				MsgVal = Mid(MsgVal, INSTR(MsgVal, Chr(34)) + 1)
				MsgVal = Left(MsgVal, INSTR(MsgVal, Chr(34)) - 1)
			END IF
			'Add message if it doesn't exist already
			MsgName = Trim(UCase(MsgName))
			T = 0
			For FindMsg = 1 To MSGC
				If MsgName = Messages(1, FindMsg) Then T = FindMsg: Exit For
			Next
			If T = 0 Then
				MSGC = MSGC + 1
				Messages(1, MSGC) = MsgName
				Messages(2, MSGC) = MsgVal
			End If
		END IF
	LOOP
	CLOSE #1
	
	'Message if no filename specified
	IF FI = "" THEN
		ShowBlock "NoPrompt"
		Temp = INKEY
		If PauseOnErr = 1 THEN
			PRINT
			PRINT "Press any key to continue"
			DO WHILE INKEY = "": SLEEP 10: LOOP
		END IF
		END
	END IF
	
	'Decide name for output file if not specified
	IF OFI = "" THEN
		OFI = FI
		For CD = LEN(OFI) TO 1 STEP -1
			IF Mid(OFI, CD, 1) = "." THEN OFI = Left(OFI, CD - 1): EXIT FOR
		Next
		OFI = OFI + ".asm"
	END IF
	
	'Find directory of source file (used for relative include)
	ProgDir = CURDIR
	IF INSTR(FI, "\") <> 0 THEN
		FOR T = LEN(FI) TO 1 STEP -1
			IF Mid(FI, T, 1) = "\" Or Mid(FI, T, 1) = "/" Then EXIT FOR
		NEXT
		ProgDir = Left(FI, T - 1)
	END IF
	IF Right(ProgDir, 1) = "\" THEN ProgDir = Left(ProgDir, LEN(ProgDir) - 1)
	IF Right(ProgDir, 1) = "/" THEN ProgDir = Left(ProgDir, LEN(ProgDir) - 1)
	
	'Load file converters
	LoadConverters
	
	'Show version
	PRINT "Great Cow BASIC (" + Version + ")"
	Print
	
	IF Dir(FI) = "" THEN
		PRINT
		PRINT Message("NoFile")
		If PauseOnErr = 1 THEN
			PRINT
			PRINT "Press any key to continue"
			DO WHILE INKEY = "": SLEEP 10: LOOP
		END IF
		END
	END IF
	
	'Start Compile
	PRINT Message("Compiling")
	
END SUB

Function IsArray (VarName As String, CurrSub As SubType Pointer) As Integer
	IsArray = 0
	Dim As Integer PD
	
	'Array var?
	FOR PD = 1 TO CurrSub->Variables
		With CurrSub->Variable(PD)
			IF .Size > 1 THEN
				IF WholeINSTR(UCase(VarName), UCase(.Name)) = 2 THEN
					IsArray = -1
					Exit Function
				End If
			END IF
		End With
	NEXT
	
	'Main sub
	FOR PD = 1 TO Subroutine(0)->Variables
		With Subroutine(0)->Variable(PD)
			IF .Size > 1 THEN
				IF WholeINSTR(UCase(VarName), UCase(.Name)) = 2 THEN
					IsArray = -1
					Exit Function
				End If
			END IF
		End With
	Next
	
END FUNCTION

Function IsNonBanked(Location As Integer) As Integer
	'Returns -1 if the location does not require banking
	'PIC only, return true on all other chips
	
	Dim As Integer CurrBank
	
	If ModePIC Then
		'Check all shared banks
		For CurrBank = 1 To NoBankLocs
			With NoBankLoc(CurrBank)
				'If the location is in a shared bank, return true
				If Location >= .StartLoc And Location <= .EndLoc Then
					Return -1
				End If
			End With
		Next
		Return 0
		
	Else
		Return -1
	End If
	
End Function

Function IsInAccessBank(VarNameIn As String) As Integer
	'Check if a specified variable is located in the access bank
	Dim As Integer FindVar
	Dim As String VarName
	
	'18F only at this stage
	If ChipFamily <> 16 Then Return 0
	
	VarName = UCase(Trim(VarNameIn))
	
	'Check for SFRs and Registers
	'Need to get location, then check if it is a non-banked location
	'Search system variable list
	For FindVar = 1 To SVC
		If UCase(Trim((SysVars(FindVar).Name))) = VarName Then
			Return IsNonBanked(SysVars(FindVar).Location)
		End If
	Next
	
	'Check if the variable being accessed is a SFR
	If IsRegister(VarNameIn) Then Return -1
	Return 0
End Function

Function IsIOPinName(PinNameIn As String) As Integer
	Dim PinName As String
	PinName = PinNameIn
	
	'Checks if PinName is that of an IO pin.
	'Format: [RP][A-Z][0-7] Or GP[0-7]
	If Len(PinName) <> 3 Then Return 0
	PinName = UCase(PinName)
	
	If Left(PinName, 1) = "R" Or Left(PinName, 1) = "P" Then
		If Mid(PinName, 2, 1) < "A" Or Mid(PinName, 2, 1) > "Z" Then Return 0
		If InStr("01234567", Mid(PinName, 3, 1)) = 0 Then Return 0
	Else
		If Left(PinName, 2) <> "GP" Then Return 0
		If InStr("01234567", Mid(PinName, 3, 1)) = 0 Then Return 0
	End If
	
	Return -1
End Function

Function IsIOReg (RegNameIn As String) As Integer
	Dim RegName As String
	Dim As Integer SD
	
	'Check if a register is in the IO space
	RegName = TRIM(UCASE(RegNameIn))
	
	'Search, return true if found
	For SD = 1 to SVC
		If SysVars(SD).Name = RegName Then
			If ModePIC Then
				Return -1
			ElseIf ModeAVR Then
				If SysVars(SD).Location > 63 Then
					Return 0
				Else
					Return -1
				End If
			End If
		End If
	Next
	
	'Check aliases
	'May have an alias to an IO register
	Dim As String Source, Temp
	Dim As Integer CurrSub, PD
	Source = Trim(UCase(RegName))
	Temp = ""
	For CurrSub = 0 To SBC
		FOR PD = 1 TO Subroutine(CurrSub)->Variables
			If UCase(Subroutine(CurrSub)->Variable(PD).Name) = Source THEN
				'Have found an alias?
				Temp = Subroutine(CurrSub)->Variable(PD).Alias
				If Temp <> "" Then
					If InStr(Temp, ",") <> 0 Then Temp = Trim(Left(Temp, InStr(Temp, ",") - 1))
					If IsIOReg(Temp) Then
						Return -1
					Else
						GoTo CheckNextSub
					End If
				End If
			End If
		Next
		
		CheckNextSub:
	Next
	
	Return 0
End Function

Function IsLowIOReg (RegNameIn As String) As Integer
	Dim RegName As String
	Dim As Integer SD
	
	'Check if a register is in the IO space
	RegName = TRIM(UCASE(RegNameIn))
	
	'Search, return true if found
	For SD = 1 to SVC
		If SysVars(SD).Name = RegName Then
			If ModePIC Then
				Return -1
			ElseIf ModeAVR Then
				If SysVars(SD).Location > 31 Then
					Return 0
				Else
					Return -1
				End If
			End If
		End If
	Next
	
	'Check aliases
	'May have an alias to a low IO register
	Dim As String Source, Temp
	Dim As Integer CurrSub, PD
	Source = Trim(UCase(RegName))
	Temp = ""
	For CurrSub = 0 To SBC
		FOR PD = 1 TO Subroutine(CurrSub)->Variables
			If UCase(Subroutine(CurrSub)->Variable(PD).Name) = Source THEN
				Temp = Subroutine(CurrSub)->Variable(PD).Alias
				'Have found an alias?
				If Temp <> "" Then
					If InStr(Temp, ",") <> 0 Then Temp = Trim(Left(Temp, InStr(Temp, ",") - 1))
					Return IsLowIOReg(Temp)
				End If
			End If
		Next
	Next
	
	Return 0
End Function

Function IsLowRegister(VarName As String) As Integer
	'Note, this can only be run accurately after AllocateRAM has run
	'AVR only
	'Will return true if register is low (r0 to r15 inclusive)
	Dim As String RealName
	Dim As Integer CurrItem, RegNo
	
	If Not ModeAVR Then Return 0
	
	'If we have an alias, get the real name
	RealName = VarName
	SearchAliasAgain:
	For CurrItem = 1 To FALC
		If RealName = FinalAliasList(CurrItem).Name Then
			RealName = FinalAliasList(CurrItem).Value
			GoTo SearchAliasAgain 'May have a nested alias
		End If
	Next
	
	'Is it low?
	For CurrItem = 1 To FRLC
		If RealName = FinalRegList(CurrItem).Name Then
			RegNo = Val(Mid(FinalRegList(CurrItem).Value, 2))
			If RegNo < 16 Then
				Return -1
			Else
				Return 0
			End If
		End If
	Next
	
	'Default to most restrictive but safest option
	Return -1
End Function

Function IsRegister (VarName As String) As Integer
	Dim As String Temp, Source
	Dim As Integer PD, CurrSub
	
	'System vars that are always registers
	'SysTemp vars are only registers on AVR, no room to be registers on PIC
	If ModeAVR Then
		IF UCase(Left(VarName, 7)) = "SYSTEMP" Then Return -1
		IF UCase(VarName) = "SYSVALUECOPY" Then Return -1
	End If
	If ModePIC Then
		If UCase(VarName) = "SYSW" Then Return -1
		If UCase(VarName) = "SYSSTATUS" Then Return -1
	End If
	IF UCase(Left(VarName, 11)) = "SYSCALCTEMP" Then Return -1
	
	IF UCase(Left(VarName, 11)) = "SYSBYTETEMP" Then Return -1
	IF UCase(Left(VarName, 11)) = "SYSWORDTEMP" Then Return -1
	IF UCase(Left(VarName, 14)) = "SYSINTEGERTEMP" Then Return -1
	IF UCase(Left(VarName, 11)) = "SYSLONGTEMP" Then Return -1
	
	IF UCase(Left(VarName, 10)) = "SYSDIVMULT" Then Return -1
	IF UCase(Left(VarName, 11)) = "SYSWAITTEMP" Then Return -1
	IF UCase(Left(VarName, 11)) = "SYSWAITTEMP" OR UCase(Left(VarName, 9)) = "DELAYTEMP" Then Return -1
	If UCase(Left(VarName, 8)) = "SYSREADA" Then Return -1
	If UCase(Left(VarName, 9)) = "SYSSTRING" Then
		Temp = UCase(Mid(VarName, 10))
		If Temp = "A" Or Temp = "A_H" Or Temp = "B" Or Temp = "B_H" Or Temp = "LENGTH" Then
			Return -1
		End If
		Return 0
	End If
	IF UCase(VarName) = "SYSDIVLOOP" Then Return -1
	IF UCase(VarName) = "SYSSIGNBYTE" Then Return -1
	
	'User defined register vars
	'If it's a register in one sub, it will be in all
	Source = Trim(UCase(VarName))
	Temp = ""
	For CurrSub = 0 To SBC
		FOR PD = 1 TO Subroutine(CurrSub)->Variables
			IF UCase(Subroutine(CurrSub)->Variable(PD).Name) = Source THEN
				Temp = Subroutine(CurrSub)->Variable(PD).Pointer
				If Left(Temp, 8) = "REGISTER" Then Return -1
			END IF
		Next
	Next
	
	'Check aliases
	'May have an alias to a register
	Source = Trim(UCase(VarName))
	Temp = ""
	For CurrSub = 0 To SBC
		FOR PD = 1 TO Subroutine(CurrSub)->Variables
			If UCase(Subroutine(CurrSub)->Variable(PD).Name) = Source THEN
				Temp = Subroutine(CurrSub)->Variable(PD).Alias
				'Have found an alias?
				If Temp <> "" Then
					If InStr(Temp, ",") <> 0 Then Temp = Trim(Left(Temp, InStr(Temp, ",") - 1))
					Return IsRegister(Temp)
				End If
			End If
		Next
	Next
	
	Return 0
End Function

FUNCTION IsString (InData As String, CurrSub As SubType Pointer) As Integer
	IsString = 0
	Dim As Integer PD
	
	'String constant?
	IF INSTR(InData, "$") <> 0 OR INSTR(InData, ";STRING") <> 0 THEN Return -1

	'String var?
	'FOR PD = 1 TO CurrSub->Variables
	'	'IF WholeINSTR(UCase(InData), UCase(Variables(PD).Name)) = 2 THEN
	'	'	IF UCase(Variables(PD).Type) = "STRING" THEN IsString = -1: Exit Function
	'	'END IF
	'	IF UCase(CurrSub->Variable(PD).Type) = "STRING" THEN
	'		IF WholeINSTR(UCase(InData), UCase(CurrSub->Variable(PD).Name)) = 2 THEN Return -1
	'	END IF
	'NEXT
	If TypeOfVar(InData, CurrSub) = "STRING" Then Return -1
END Function

Function IsUnaryOp (InData As String) As Integer
	
	If InData = "-" Then Return -1
	If InData = "!" Then Return -1
	Return 0
	
End Function

FUNCTION IsWord (InData As String, CurrentSub As Integer) As Integer
	
	Dim As String Temp, SubParam
	Dim As Integer PD, T, FindFn
	Dim As SubType Pointer CurrSub, MainSub
	CurrSub = Subroutine(CurrentSub)
	MainSub = Subroutine(0)
	
	IsWord = 0
	
	'Word mode forced?
	IF INSTR(UCase(InData), "[WORD]") <> 0 THEN Return -1
	IF Trim(InData) = "" THEN Exit Function
	
	'Check for word function
	For FindFn = 1 To SBC
		If Subroutine(FindFn)->IsFunction Then
			IF WholeINSTR(UCase(InData), UCase(Subroutine(FindFn)->Name)) = 2 And UCASE(Subroutine(FindFn)->ReturnType) = "WORD" Then
				Return -1
			End If
		End If
	Next
	
	'To proceed any further, sub needs to have dim commands compiled
	If Not CurrSub->VarsRead Then CompileDim(CurrSub)
	
	'Check for local word var
	FOR PD = 1 TO CurrSub->Variables
		With CurrSub->Variable(PD)
			IF WholeINSTR(UCase(InData), UCase(.Name)) = 2 THEN
				IF UCase(.Type) = "WORD" Then
					Return -1
				Else
					'It's not a word in this sub, ignore global type
					Return 0
				End If
			END IF
		End With
	Next
	
	'Check for global word var
	FOR PD = 1 TO MainSub->Variables
		With MainSub->Variable(PD)
			IF WholeINSTR(UCase(InData), UCase(.Name)) = 2 THEN
				IF UCase(.Type) = "WORD" Then Return -1
			END IF
		End With
	Next
	
	Return 0
END FUNCTION

Sub LoadConverters
	'Loads file format converters
	Dim As String SaveDir, ConvDir, CurrFile, InLine, InName, InVal
	Dim As Integer f
	
	'Change to directory
	SaveDir = CurDir
	#Ifdef __FB_LINUX__
		ConvDir = ID + "/converters" 
	#ELSE
		ConvDir = ID + "\converters"
	#EndIf
	If ChDir(ConvDir) = 0 Then
		'Read each .ini file in directory
		CurrFile = Dir("*.ini")
		Do While CurrFile <> ""
			
			'Create new FileConverter
			FileConverters += 1
			With FileConverter(FileConverters)
				.Name = CurrFile
				.InFormats = 0
				
				f = FreeFile
				Open CurrFile For Input As #f
				Do While Not Eof(f)
					Line Input #f, InLine
					If InStr(InLine, "=") <> 0 Then
						InName = LCase(Trim(Left(InLine, InStr(InLine, "=") - 1)))
						InVal = Trim(Mid(InLine, InStr(InLine, "=") + 1))
						
						'Process line
						If InName = "desc" Then
							.Desc = InVal
						ElseIf InName = "in" Then
							GetTokens(InVal, .InFormat(), .InFormats)
						ElseIf InName = "out" Then
							.OutFormat = InVal
						ElseIf InName = "exe" Then
							.ExeName = InVal
						End If
					End If
				Loop
				Close #f
			End With
			
			CurrFile = Dir
		Loop
		
	End If
	
	'Restore directory
	ChDir SaveDir
	
End Sub

FUNCTION LocationOfSub (SubNameIn As String, SubSigIn As String, Origin As String = "", AllowVague As Integer = 0) As Integer
	
	'SubNameIn - name of sub to find
	'SubSigIn - signature of sub to find, leave blank to find all. If blank, may
	'           need to also set AllowVague to suppress error if multiple
	'           overloaded subs match name
	'Origin - Location sub is called from. Used for error reporting. Optional
	'AllowVague - Set to -1 to suppress error if multiple overloaded subs are
	'             found and no signature provided. First matching sub will be
	'             returned. 
	
	'Returns:
	' Location in Subroutines() of match
	' 0 if no match
	' -1 if overloaded, and couldn't determine which overload matches
	
	Dim As String SubName, SubSig, Temp, ParamTemp, ErrorTemp
	Dim As Integer T, FoundSameName, BestMatch, BestMatchLoc, ThisScore
	
	SubName = UCase(LTrim(SubNameIn))
	SubSig = SubSigIn
	IF Left(SubName, 5) = "CALL " THEN SubName = Trim(Mid(SubName, 6))
	LocationOfSub = 0
	
	'If a var is being set, don't bother checking to see if the var is a sub
	IF Mid(SubName, INSTR(SubName, " ") + 1, 1) = "=" THEN EXIT FUNCTION
	
	'Remove any parameters from the name
	IF INSTR(SubName, "(") <> 0 THEN SubName = Left(SubName, INSTR(SubName, "(") - 1)
	IF INSTR(SubName, " ") <> 0 THEN SubName = Left(SubName, INSTR(SubName, " ") - 1)
	
	'Exit if SubName is a known, common command
	'This code is meant to save time, so no point in checking for common commands if they are not subs
	IF SubName = "IF" THEN EXIT FUNCTION
	IF SubName = "DO" THEN EXIT FUNCTION
	IF SubName = "SET" THEN EXIT FUNCTION
	IF SubName = "DIR" THEN EXIT FUNCTION
	IF SubName = "FOR" THEN EXIT FUNCTION
	IF SubName = "DIM" THEN EXIT FUNCTION
	IF SubName = "END" THEN EXIT FUNCTION
	IF SubName = "LOOP" THEN EXIT FUNCTION
	IF SubName = "NEXT" THEN EXIT FUNCTION
	IF SubName = "WAIT" THEN EXIT FUNCTION
	If Left(SubName, 5) = "ENDIF" Then Exit Function
	
	'Dodgy hack for SysReadString and SysCopyString to allow entry to middle of subroutine
	If SubName = "SYSREADSTRINGPART" Or SubName = "SYSCOPYSTRINGPART" Then
		SubName = Left(SubName, Len(SubName) - 4)
	End If
	
	'Returns the position of DataSource in SUBDATA()
	SubName = Trim(SubName)
	FoundSameName = 0
	
	BestMatch = 0
	BestMatchLoc = -2
	
	FOR T = 1 TO SBC
		Temp = UCase(Trim(Subroutine(T)->Name))
		
		If Temp = SubName THEN
			
			'Early exit if sub not overloaded
			If Not Subroutine(T)->Overloaded Or AllowVague Then
				Return T
			End If
			
			FoundSameName = -1
			If SubSig <> "" Then
				ThisScore = SubSigMatch(GetSubSig(*Subroutine(T)), SubSig) 
				If ThisScore > BestMatch Then
					BestMatch = ThisScore
					BestMatchLoc = T
				End If
			Else
				'Handle calls to overloaded subs (ie call PRINT60 to call sub #60)
				'Exact sig will not be known
				If Len(SubName) > Len(Temp) Then
					If Left(SubName, Len(Temp)) = Temp Then
						Return Val(Mid(SubName, Len(Temp) + 1))
					End If
				
				'No number, so we've got an ambigious sub reference, error
				'If this happens, there is a bug in the compiler
				'Or there is a sub with no params
				Else
					'Sub may not actually have any parameters
					If Subroutine(T)->ParamCount = 0 Then
						Return T
						
					Else
						'Print "Internal error, ambigiuous sub reference"
						'Print "Sub: " + SubName + ", signature needed but not given"
						ErrorTemp = Message("DuplicateSub")
						Replace ErrorTemp, "%sub%", SubName
						LogError ErrorTemp, Origin
						Return -1 'Should cause a runtime error, giving location of bug
					End If
				End If
			End If
		END If
		
	NEXT
	
	'If no matches, check for full name match
	If BestMatchLoc = -2 Then
		For T = 1 TO SBC
			'Temp = UCase(Trim(Subroutine(T)->Name))
			Temp = UCase(Trim(GetSubFullName(T)))
			If Temp = SubName Then
				'On first match, set best match loc
				If BestMatchLoc = -2 Then
					BestMatchLoc = T
				
				'On second match, return to error state
				Else
					BestMatchLoc = 0
					Exit For
				End If
				
			End If
		Next
		
		'If still couldn't find match, set BestMatchLoc back to 0.
		If BestMatchLoc = -2 Then BestMatchLoc = 0
	End If
	
	If BestMatchLoc = 0 And FoundSameName Then Return -1 'Found same name but not same sig, error
	Return BestMatchLoc 'Found same name and sig, or found no match
	
END FUNCTION

Sub LogError(InMessage As String, Origin As String = "")
	ErrorsFound = -1
	LogOutputMessage Origin + InMessage + "E"
End Sub

Sub LogOutputMessage(InMessage As String)
	
	Dim As Integer FindDup
	
	'Check if message is duplicate
	For FindDup = 1 To OutMessages
		If InMessage = OutMessage(FindDup) Then
			'Duplicate found, exit
			Exit Sub
		End If
	Next
	
	'Error if too many messages
	If OutMessages >= MAX_OUTPUT_MESSAGES - 1 Then
		'If too many warnings, just exit
		If Right(InMessage, 1) = "W" Then Exit Sub
		
		'If too many errors, error and quit
		If OutMessages = (MAX_OUTPUT_MESSAGES - 1) Then
			OutMessages += 1
			OutMessage(OutMessages) = Message("TooManyErrors") + "E"
		End If
		
		Return
	End If
	
	'Log message
	OutMessages += 1
	OutMessage(OutMessages) = InMessage
	
End Sub

Sub LogWarning(InMessage As String, Origin As String = "")
	LogOutputMessage Origin + InMessage + "W"
End Sub

Sub OptimiseCalls
	
	'Ensure that rcall/call and rjmp/jmp/goto are used appropriately
	'Will measure the distance between the jump and the target. If it's small
	'enough for a relative call, the non-relative jump will be replaced with a
	'relative one to make the program smaller. If there is already a relative
	'jump, but the distance is too great, a non-relative jump will be inserted
	'instead.
	
	'This routine is AVR and PIC18F compatible only
	If ChipFamily <> 16 And Not ModeAVR Then Exit Sub
	
	Dim As LinkedListElement Pointer CurrLine, LabelList, LabelListPos
	Dim As String JumpTarget, ProperCmd, CheckTarget, NextLine, TempLoc
	Dim As Integer IsRelative, UseRelative, IsJump, LineSize, CurrLinePos
	Dim As Integer JumpSize, IsFirstLine
	
	'Get list of labels and locations in program
	CurrLine = CompilerOutput->CodeList->Next
	CurrLinePos = 0
	LabelList = LinkedListCreate
	LabelListPos = LabelList
	Do While CurrLine <> 0
		
		LineSize = CalcLineSize(CurrLine->Value, 1)
		If LineSize = 0 Then
			If ModeAVR Then
				If Right(CurrLine->Value, 1) = LabelEnd Then
					LabelListPos = LinkedListInsert(LabelListPos, UCase(Left(CurrLine->Value, Len(CurrLine->Value) - 1)))
					LabelListPos->NumVal = CurrLinePos
				End If
			ElseIf ChipFamily = 16 Then
				If CurrLine->Value <> "" And Left(CurrLine->Value, 1) <> ";" And Left(CurrLine->Value, 1) <> " " And Left(CurrLine->Value, 9) <> "PRESERVE " Then
					LabelListPos = LinkedListInsert(LabelListPos, UCase(CurrLine->Value))
					LabelListPos->NumVal = CurrLinePos
				End If
			End If
		Else
			CurrLinePos += LineSize
		End If
		
		CurrLine = CurrLine->Next
	Loop
	
	'Check program, make changes where needed
	CurrLine = CompilerOutput->CodeList->Next
	CurrLinePos = 0
	Do While CurrLine <> 0
		
		'Get current position
		If CurrLinePos = 0 Then
			IsFirstLine = -1
		End If
		CurrLinePos += CalcLineSize(CurrLine->Value, 1)
		
		'Check to see if command is call or goto
		IsJump = 0
		If ModeAVR Then
			If Left(CurrLine->Value, 7) = " rcall " Or Left(CurrLine->Value, 6) = " call " Then
				IsJump = 1
			ElseIf Left(CurrLine->Value, 6) = " rjmp " Or Left(CurrLine->Value, 5) = " jmp " Then
				IsJump = 2
			End If
		ElseIf ChipFamily = 16 Then
			If Left(CurrLine->Value, 7) = " rcall " Or Left(CurrLine->Value, 6) = " call " Then
				IsJump = 1
			ElseIf Left(CurrLine->Value, 6) = " goto " Or Left(CurrLine->Value, 5) = " bra " Then
				IsJump = 2
			End If
		End If
		
		If IsJump <> 0 Then
			'Get jump target
			JumpTarget = Trim(CurrLine->Value)
			JumpTarget = Mid(JumpTarget, InStr(JumpTarget, " ") + 1)
			If InStr(JumpTarget, ";") <> 0 Then JumpTarget = Trim(Left(JumpTarget, InStr(JumpTarget, ";") - 1))
			'Check if relative
			IsRelative = 0
			If ModeAVR Then
				If Left(CurrLine->Value, 2) = " r" Then IsRelative = -1
			ElseIf ChipFamily = 16 Then
				If Left(CurrLine->Value, 2) = " r" Or Left(CurrLine->Value, 2) = " b" Then IsRelative = -1
			End If
			
			'If chip has 8192 or less bytes program memory, use r* exclusively
			'Otherwise, alter as needed
			If (ModeAVR And ChipProg <= 8192) Or (ChipFamily = 16 And ChipProg < 2048) Then
				UseRelative = -1
			Else
				UseRelative = 0
				
				'Get label
				CheckTarget = Trim(UCase(JumpTarget))
				'Search 2048 words either way from current location
				'If label found, then can use relative mode
				If CheckTarget = "$" Then
					UseRelative = -1
					
				ElseIf ModeAVR And WholeInstr(CheckTarget, "PC") = 2 Then
					TempLoc = CheckTarget
					WholeReplace TempLoc, "PC", "0"
					Calculate TempLoc
					If Abs(Val(TempLoc)) < 2048 Then
						'Print "Using relative for "; CheckTarget
						UseRelative = -1
					End If 
					
				'For bootloader compatibility, treat first line as non-relative on all
				'PIC 18F chips.
				ElseIf IsFirstLine And ModePIC Then
					UseRelative = 0
					
				Else
					LabelListPos = LabelList->Next
					Do While LabelListPos <> 0
						If LabelListPos->Value = CheckTarget Then
							JumpSize = Abs(LabelListPos->NumVal - CurrLinePos)
							If (ModeAVR And JumpSize < 2048) Or (ChipFamily = 16 And JumpSize < 1024) Then
								UseRelative = -1
							End If
							Exit Do
						End If
						LabelListPos = LabelListPos->Next
					Loop
				End If
				
			End If
			
			'If using relative when shouldn't or not when should, change line
			If IsRelative <> UseRelative Then
				If ModeAVR Then
					If UseRelative Then
						ProperCmd = " r"
					Else
						ProperCmd = " "
					End If
					If IsJump = 1 Then
						ProperCmd += "call "
					Else
						ProperCmd += "jmp "
					End If
				
				ElseIf ChipFamily = 16 Then
					If UseRelative Then
						If IsJump = 1 Then
							ProperCmd = " rcall "
						Else
							ProperCmd = " bra "
						End If
					Else
						If IsJump = 1 Then
							ProperCmd = " call "
						Else
							ProperCmd = " goto "
						End If
					End If
				End If
				CurrLine->Value = ProperCmd + JumpTarget
			End If
			
		End If
		
		CurrLine = CurrLine->Next
		IsFirstLine = 0
	Loop
	
	DeAllocate LabelList
	
End Sub

SUB OptimiseIF (CompSub As SubType Pointer = 0)
	'Optimise compiled IF statements. Replace this:
	' conditional skip
	' goto LABEL
	' single instruction
	' LABEL
	'With this:
	' opposite conditional skip
	' single instruction
	'This is called at the end of the program, as the final step of compilation.
	'(CompSub = 0, output code optimised)
	'If may also be called before page selection has been added. If this happens,
	'it is possible that single call instructions will become multiple instruction
	'page select, call, page select. If being called before page selection,
	'CompSub will be non-zero, and so calls should not be optimised yet.
	
	Dim As String Temp, InLine
	Dim As Integer T
	Dim As LinkedListElement Pointer CurrLine, PL(3)
	
	If CompSub = 0 Then
		CurrLine = CompilerOutput->CodeList->Next
	Else
		CurrLine = CompSub->CodeStart->Next
	End If
	
	Do While CurrLine <> 0
		Temp = CurrLine->Value
		
		'Find skip > goto > other > label, replace with opposite skip, other
		
		IF Left(Temp, 5) = "ENDIF" THEN
			If Right(Temp, 1) = ":" THEN Temp = Left(Temp, LEN(Temp) - 1)
			
			'Get 3 lines before CurrLine
			PL(0) = CurrLine
			For T = 1 To 3
				If PL(T - 1) <> 0 Then PL(T) = PL(T - 1)->Prev
				'Remove comments from list
				Do While Left(PL(T)->Value, 9) = "PRESERVE "
					If PL(T)->Prev <> 0 Then
						PL(T) = PL(T)->Prev
					Else
						Exit Do
					End If
				Loop
			Next
			
			'Before:		After:
			'3 btfsc		btfss
			'2 goto		inst
			'1 inst
			'0 label < Looking at this
			
			'If compiling a subroutine, check if instruction is call
			If CompSub <> 0 Then
				If Left(PL(1)->Value, 6) = " call " Or Left(PL(1)->Value, 7) = " rcall " Then
					'Call found, need to skip
					GoTo OptimiseNextIf
				EndIf
			End If
			
			If ModePIC Then
				IF PL(2)->Value = " goto " + Temp Or PL(2)->Value = " bra " + Temp Then
					InLine = Left(PL(3)->Value, 6)
					IF InLine = " btfsc" THEN T = 0
					IF InLine = " btfss" THEN T = 1
					IF T = 0 THEN Replace PL(3)->Value, "btfsc", "btfss"
					IF T = 1 THEN Replace PL(3)->Value, "btfss", "btfsc"
					LinkedListDelete(PL(2))
					CurrLine = LinkedListDelete(CurrLine)
				END If
				
			ElseIf ModeAVR Then
				IF PL(2)->Value = " rjmp " + Temp Or PL(2)->Value = " jmp " + Temp THEN
					InLine = Left(PL(3)->Value, 5)
					T = -1
					IF InLine = " sbrc" THEN T = 0
					IF InLine = " sbrs" THEN T = 1
					IF InLine = " sbic" THEN T = 2
					IF InLine = " sbis" THEN T = 3
					IF InLine = " brbc" THEN T = 4
					IF InLine = " brbs" THEN T = 5
					IF T = 0 THEN Replace PL(3)->Value, "sbrc", "sbrs"
					IF T = 1 THEN Replace PL(3)->Value, "sbrs", "sbrc"
					IF T = 2 THEN Replace PL(3)->Value, "sbic", "sbis"
					IF T = 3 THEN Replace PL(3)->Value, "sbis", "sbic"
					IF T = 4 THEN Replace PL(3)->Value, "brbc", "brbs"
					IF T = 5 THEN Replace PL(3)->Value, "brbs", "brbc"
					If T <> -1 Then
						LinkedListDelete(PL(2))
						CurrLine = LinkedListDelete(CurrLine)
					End If
				END IF
			End If
			
			OptimiseNextIf:
		END If
		
		CurrLine = CurrLine->Next
	Loop
	
END SUB

Sub PreparePageData
	'Generate prog mem page data
	Dim As Integer PageSize, FirstPageStart, FindLastVector, LastVector, LastLoc, IsFirstPage
	
	PageSize = 0
	
	If ModePIC Then
		'On low end, have 512 word pages
		If ChipFamily = 12 Then
			PageSize = 512
			If HasSFR("INTCON") Then
				FirstPageStart = 5
			Else
				FirstPageStart = 0
			End If
			
		'Midrange, 2048 words
		'First page starts at location 5, after interrupt
		ElseIf ChipFamily = 14 Or ChipFamily = 15 Then
			PageSize = 2048
			If HasSFR("INTCON") Then
				FirstPageStart = 5
			Else
				FirstPageStart = 0
			End If
		
		'18F, single page
		'First page starts at location 12, after interrupt
		ElseIf ChipFamily = 16 Then
			FirstPageStart = 12
			
		End If
	ElseIf ModeAVR Then
		'No pages
		PageSize = 0
		'Find last interrupt vector
		LastVector = 0
		For FindLastVector = 1 To IntCount
			With Interrupts(FindLastVector)
				If .VectorLoc > LastVector Then LastVector = .VectorLoc
			End With
		Next
		FirstPageStart = LastVector + 2
		
	ElseIf ModeZ8 Then
		'Single page?
		PageSize = 0
		LastVector = 0
		For FindLastVector = 1 To IntCount
			With Interrupts(FindLastVector)
				If .VectorLoc > LastVector Then LastVector = .VectorLoc
			End With
		Next
		FirstPageStart = LastVector + 1
	End If
	'Generate page info
	LastLoc = 0
	Do While LastLoc < ChipProg
		ProgMemPages += 1
		With ProgMemPage(ProgMemPages)
			'Set first location in page
			If ProgMemPages = 1 Then
				.StartLoc = FirstPageStart + Bootloader
			Else
				.StartLoc = LastLoc
			End If
			
			'Calc and set last location
			If PageSize = 0 Then
				LastLoc = ChipProg
			Else
				LastLoc += PageSize
				If LastLoc > ChipProg Then LastLoc = ChipProg
			End If
			.EndLoc = LastLoc - 1
			.CodeSize = 0
			.MaxSize = .EndLoc - .StartLoc + 1
		End With
	Loop
	
End Sub

SUB ProcessArrays (CompSub As SubType Pointer)
	Dim As String InLine, Origin, Temp, AV, ArrayName, ArrayType, ArrayPosition
	Dim As String ArrayHandler, AppendArrayPosition
	Dim As String NewCode(20)
	Dim As Integer ATV, ArraysInLine, CD, LastArray, AF, SS, UseTempVar
	Dim As Integer ArrayDir, L, P, ArrayPointer, NCO, MarkBlock
	Dim As LinkedListElement Pointer CurrLine, NewCodeList, NewCodeLine
	
	Dim As SubType Pointer MainSub
	MainSub = Subroutine(0)
	
	'Combine local arrays and global arrays into single list
	Dim As VariableType Pointer Variable(MainSub->Variables + CompSub->Variables)
	Dim As Integer Variables
	Variables = 0
	For CD = 1 To MainSub->Variables
		If MainSub->Variable(CD).Size > 1 Then
			Variables += 1
			Variable(Variables) = @(MainSub->Variable(CD))
		End If
	Next
	For CD = 1 To CompSub->Variables
		If CompSub->Variable(CD).Size > 1 Then
			Variables += 1
			Variable(Variables) = @(CompSub->Variable(CD))
		End If
	Next
	
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		InLine = CurrLine->Value
		
		'No brackets means no arrays in line
		If Instr(InLine, "(") = 0 Then Goto CompileArraysNextLine
		
		'Some things will look like arrays but are not
		If (WholeINSTR(InLine, "low") = 2 And InStr(InLine, "low(") <> 0) Or (WholeINSTR(InLine, "high") = 2 And InStr(InLine, "high(") <> 0) Then Goto CompileArraysNextLine
		
		Origin = ""
		IF INSTR(InLine, ";?F") <> 0 THEN 
			Origin = Mid(InLine, INSTR(InLine, ";?F"))
			InLine = RTrim(Left(InLine, INSTR(InLine, ";?F") - 1))
		END IF
		ATV = 0 'Number of array temporary variables
		
		'Mark array operation as block?
		MarkBlock = 0
		If Left(InLine, 3) = "DO " Or Left(InLine, 5) = "WAIT " Or Left(InLine, 5) = "LOOP " Then MarkBlock = -1
		
		'Count the number of arrays in the line
		ArraysInLine = 0
		Temp = InLine
		'Local arrays
		FOR CD = 1 TO Variables
			With *Variable(CD)
				If .Size > 1 Then
					Do While WholeINSTR(Temp, .Name) = 2 AND INSTR(Temp, "(") <> 0
						ArraysInLine += 1
						WholeReplace Temp, "[1]" + .Name, ""
						'Print .Name, ArraysInLine, Temp
					Loop
				END IF
			End With
		Next
		
		'Does the line contain an array?
		LastArray = 1
CheckArrayAgain:
		AF = 0
		FOR CD = LastArray TO Variables
			With *Variable(CD)
				If .Size > 1 Then
					IF WholeINSTR(InLine, .Name) = 2 AND INSTR(InLine, "(") <> 0 THEN
						IF WholeINSTR(InLine, .Name + "()") = 2 THEN
							AF = 0
						Else
							AF = CD
							EXIT FOR
						End If
					END IF
				END IF
			End With
		NEXT

		'Show error if array has not been found but brackets have
		IF AF = 0 AND INSTR(InLine, "(") <> 0 AND INSTR(InLine, "()") = 0 AND INSTR(InLine, "$") = 0 AND Left(InLine, 6) <> " retlw" THEN
			InLine = Left(InLine, INSTR(InLine, "(") - 1)
			'Get name of just array/function
			FOR SS = LEN(InLine) TO 1 STEP -1
				IF Mid(InLine, SS, 1) = " " THEN InLine = Mid(InLine, SS + 1): Exit For
			NEXT
			'High and Low are built in assembler function on AVR, ignore them
			If ModeAVR And (UCase(Trim(InLine)) = "LOW" OR UCase(Trim(InLine)) = "HIGH") Then Goto CompileArraysNextLine
			'Casting operations
			If Left(InLine, 1) = "[" And Right(InLine, 1) = "]" Then Goto CompileArraysNextLine
			
			'Check if brackets follow a calculation symbol, show error if they don't
			IF InLine <> "" THEN
				IF Not IsCalc(RIGHT(InLine, 1)) THEN
					Temp = Message("ArrayNoDec")
					Replace Temp, "%Name%", InLine
					LogError Temp, Origin
				End IF
			END IF
		END IF
		
		'Array has been found, so generate code to access it
		IF AF <> 0 THEN
			
			'Get array type
			With *Variable(AF)
				ArrayName = .Name
				ArrayType = .Type
			End With
			If ArrayType = "STRING" Then ArrayType = "BYTE"
			UseTempVar = -1
			If ArraysInLine = 1 And ArrayType = "BYTE" Then UseTempVar = 0
			
			NewCodeList = LinkedListCreate
			NewCodeLine = NewCodeList
			
			If MarkBlock Then
				NewCodeLine = LinkedListInsert(NewCodeLine, ";BLOCKSTART," + ArrayName)
			End If
			
			'Is array being read or set?
			ArrayDir = 0 '0 = read, 1 = set
			If UseTempVar Then
				'These commands set variables:
				IF Left(InLine, 4) = "SET " THEN ArrayDir = 1
				'IF INSTR(InLine, "=") > INSTR(InLine, ArrayName) THEN ArrayDir = 1
				IF INSTR(InLine, "=") <> 0 Then
					'If array name before =, is being set
					Temp = Left(InLine, InStr(InLine, "=") - 1)
					If WholeINSTR(Temp, ArrayName) = 2 Then ArrayDir = 1 
				End If
				If LCase(Left(LTrim(InLine), 6)) = "movwf " THEN ArrayDir = 1
				'These commands read variables, but may appear to set them based on above
				IF Left(InLine, 3) = "IF " THEN ArrayDir = 0
				IF Left(InLine, 3) = "DO " THEN ArrayDir = 0
				IF Left(InLine, 5) = "WAIT " THEN ArrayDir = 0
				IF Left(InLine, 10) = "READTABLE " Then
					Temp = InLine
					Replace Temp, ",", ""
					IF INSTR(Temp, ",") < INSTR(Temp, ArrayName) THEN ArrayDir = 1
				END IF
			Else
				ArrayDir = 0
			End If
				
			'Get the array name and index
			Temp = Mid(InLine, WholeInstrLoc(InLine, ArrayName))
			
			L = 1
			P = INSTR(Temp, "(")
			Do
				P += 1
				IF Mid(Temp, P, 1) = "(" THEN L = L + 1
				IF Mid(Temp, P, 1) = ")" THEN L = L - 1
			Loop While L <> 0 AND P < LEN(Temp)
			IF L <> 0 THEN
				LogError Message("BadBrackets"), Origin
			Else
				Temp = Left(Temp, P)
				
				'Get array index
				ArrayPosition = Mid(Temp, INSTR(Temp, "(") + 1)
				ArrayPosition = Left(ArrayPosition, Len(ArrayPosition) - 1)
				
				'Get array type (real/pointer)
				IF Variable(AF)->Pointer = "REAL" THEN
					ArrayHandler = "@" + ArrayName
					ArrayPointer = 0
				END IF
				IF Variable(AF)->Pointer = "POINTER" THEN
					ArrayHandler = "Sys" + ArrayName + "Handler"
					ArrayPointer = -1
				END If
				
				'Access location directly?
				If IsConst(ArrayPosition) And Not ArrayPointer Then
					'If using a fixed element of a real array, treat it as a single element variable
					'Create alias to refer to the exact memory location directly and use that
					AV = "SYS" + ArrayName + "_" + Str(MakeDec(ArrayPosition))
					AddVar AV, "BYTE", 1, CompSub, "ALIAS:" + ArrayName + " + " + Str(MakeDec(ArrayPosition)), Origin 
					'Can't put array name back into line, causes problem if array accessed twice
					WholeReplace InLine, "[1]" + Temp, TempRemove(AV)
					CurrLine->Value = InLine + Origin
					
				'Need to use indirect addressing for array
				Else
					
					'Alter line
					If UseTempVar Then
						ATV = ATV + 1
						AV = "SysArrayTemp" + Str(ATV)
						AddVar AV, ArrayType, 1, CompSub, "REAL", Origin
					Else
						If ChipFamily = 12 Or ChipFamily = 14 Then AV = "INDF"
						If ChipFamily = 15 Or ChipFamily = 16 Then AV = "INDF0"
						If ModeAVR Then AV = "SysPointerX"
					End If
					WholeReplace InLine, "[1]" + Temp, AV
					CurrLine->Value = InLine + Origin
					If ArrayPointer Then AddVar ArrayHandler, "WORD", 1, 0, "REAL", Origin
					
					'Get position to set
					If IsConst(ArrayPosition) And MakeDec(ArrayPosition) = 0 Then
						AppendArrayPosition = ""
					Else
						AppendArrayPosition = "+" + ArrayPosition
					End If
					
					'Add code to read/set array
					If ModePIC And (ChipFamily = 12 Or ChipFamily = 14) Then
						If ArrayPointer = 0 Then
							NewCodeLine = LinkedListInsert(NewCodeLine, "FSR = [byte]" + ArrayHandler + AppendArrayPosition+ Origin)
							If HighFSR Then
								NewCodeLine = LinkedListInsert(NewCodeLine, " bankisel " + ArrayName)
							End If
						Else
							NewCodeLine = LinkedListInsert(NewCodeLine, "FSR = " + ArrayHandler + AppendArrayPosition + Origin)
							If HighFSR Then
								NewCodeLine = LinkedListInsert(NewCodeLine, " bcf STATUS, IRP")
								NewCodeLine = LinkedListInsert(NewCodeLine, " btfsc " + ArrayHandler + "_H,0")
								NewCodeLine = LinkedListInsert(NewCodeLine, " bsf STATUS, IRP")
							End If
						End If
						If UseTempVar Then
							IF ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, AV + " = INDF")
							IF ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "INDF = " + AV)
						End If
						
					ElseIf ModePIC And ChipFamily = 15 Then
						NewCodeLine = LinkedListInsert(NewCodeLine, "AFSR0 = " + ArrayHandler + AppendArrayPosition + Origin)
						If UseTempVar Then
							IF ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, AV + " = INDF0")
							IF ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "INDF0 = " + AV)
						End If
						
					ElseIf ModePIC And ChipFamily = 16 Then
						If ArrayPointer = 0 Then
							If IsConst(ArrayPosition) Then
								NewCodeLine = LinkedListInsert(NewCodeLine, " lfsr 0," + ArrayName + "+" + ArrayPosition)
							Else
								NewCodeLine = LinkedListInsert(NewCodeLine, " lfsr 0," + ArrayName)
								NewCodeLine = LinkedListInsert(NewCodeLine, "AFSR0 = AFSR0 +" + ArrayPosition)
							End If
						Else
							NewCodeLine = LinkedListInsert(NewCodeLine, "AFSR0 = " + ArrayHandler + AppendArrayPosition + Origin)
						End If
						If UseTempVar Then
							IF ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, AV + " = INDF0")
							IF ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "INDF0 = " + AV)
						End If
					
					ElseIf ModeAVR Then
						AddVar "SysStringA", "WORD", 1, 0, "REAL", Origin
						If ArrayPointer = 0 Then
							NewCodeLine = LinkedListInsert(NewCodeLine, "SysStringA = @" + ArrayName + AppendArrayPosition + Origin)
						Else
							NewCodeLine = LinkedListInsert(NewCodeLine, "SysStringA = " + ArrayHandler + AppendArrayPosition + Origin)
						End If
						If UseTempVar Then
							IF ArrayDir = 0 THEN NewCodeLine = LinkedListInsert(NewCodeLine, AV + " = SysPointerX")
							IF ArrayDir = 1 THEN NewCodeLine = LinkedListInsert(NewCodeLine, "SysPointerX = " + AV)
						End If
					End If
				
				END IF
				
				If MarkBlock Then
					NewCodeLine = LinkedListInsert(NewCodeLine, ";BLOCKEND," + ArrayName)
				End If
				
				'Add array access code
				'Add after line when ArrayDir = 1, before when = 0
				If ArrayDir = 0 Then
					LinkedListInsertList(CurrLine->Prev, NewCodeList)
				Else
					LinkedListInsertList(CurrLine, NewCodeList)
				End If
			End If
				
			'Check the line again to see if it contains another array
			LastArray = AF
			GoTo CheckArrayAgain
		END IF
	
	CompileArraysNextLine:
		CurrLine = CurrLine->Next
	Loop
	
	'Deal with any missed names
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		'Replace tokens with names
		CurrLine->Value = ReplaceFnNames(CurrLine->Value)
		CurrLine = CurrLine->Next
	Loop
	
END SUB

Function PutInRegister(ByRef OutList As LinkedListElement Pointer, SourceValue As String, RegType As String, Origin As String) As String
	'Take any value, and put it into a register
	'Return name of register
	Dim As String Temp, OutVar, CurrRegType
	Dim As Integer Overwrite, ForceHighReg, UpgradeType
	
	'Try to create new list
	If OutList = 0 Then OutList = LinkedListCreate
	
	Overwrite = 0
	ForceHighReg = 0
	Do While Mid(SourceValue, 2, 1) = ":"
		'Check if register will be overwritten
		If Left(SourceValue, 2) = "O:" Then
			SourceValue = Mid(SourceValue, 3)
			Overwrite = -1
		End If
		'Check if register needs to be high
		If Left(SourceValue, 2) = "H:" Then
			SourceValue = Mid(SourceValue, 3)
			ForceHighReg = -1
		End If
	Loop

	'AVR Only
	If Not ModeAVR Then
		Return SourceValue
	End If
	
	'If no type, decide one
	CurrRegType = TypeOfValue(SourceValue, Subroutine(GetSubID(Origin)))
	If RegType = "" Then RegType = CurrRegType
	
	'Might need to upgrade type
	UpgradeType = 0
	If CastOrder(RegType) > CastOrder(CurrRegType) Then UpgradeType = -1
	
	'If already a register, then return name
	If IsRegister(SourceValue) And Not Overwrite And Not ForceHighReg Then
		'Might need to upgrade type of register
		If UpgradeType Then
			IF Left(UCase(SourceValue), 7) = "SYSTEMP" Then
				'Need to upgrade neatly
				If CurrRegType = "BYTE" And RegType = "WORD" Then
					LinkedListInsert(OutList, " clr " + GetByte(SourceValue, 1))
				End If
				UpgradeCalcVar SourceValue, RegType
				Return SourceValue
			End If
		Else
			Return SourceValue
		End If
	End If
	
	'Create temp var
	Temp = ""
	If IsConst(SourceValue) Or ForceHighReg Then Temp = "H:"
	OutVar = GetCalcVar(Temp + RegType)
	
	'Copy value
	'OutList = CompileVarSet(SourceValue, OutVar, Origin)
	LinkedListInsertList(OutList, CompileVarSet(SourceValue, OutVar, Origin)) 
	Return OutVar
	
End Function

SUB ReadChipData
	Dim As String TempData, TempList(20)
	Dim As String ReadDataMode, ChipDataFile, InLine
	Dim As String PinName, PinDir
	Dim As PinDirType Pointer PinDirData
	Dim As Integer TDC, CW, PD
	Dim As ConfigSetting Pointer ThisSetting
	Dim As LinkedListElement Pointer CurrLoc
	
	'Get filename
#IFDEF __FB_LINUX__
	ChipDataFile = ID + "/chipdata/" + LCase(ChipName) + ".dat"
#ELSE
	ChipDataFile = ID + "\chipdata\" + ChipName + ".dat"
#ENDIF
  
	IF VBS = 1 THEN PRINT SPC(10); ChipDataFile
	
	IF ChipFamily = 16 THEN ConfWords = 0
	
	'Check that the chip data is present
	If OPEN(ChipDataFile For Input As #1) <> 0 Then
		LogError Message("ChipNotSupported")
		WriteErrorLog
		END
	End If
	
	ReadDataMode = ""
	DO WHILE NOT EOF(1)
		LINE INPUT #1, InLine
		InLine = Trim(LCase(InLine))
		IF InLine = "" THEN Goto ReadNextChipInfoLine
		IF Left(InLine, 1) = "'" THEN Goto ReadNextChipInfoLine
		
		IF Left(InLine, 1) = "[" AND Right(InLine, 1) = "]" THEN
			ReadDataMode = InLine
		
		'Chip Data
		ElseIF ReadDataMode = "[chipdata]" AND INSTR(InLine, "=") <> 0 THEN
			TempData = Trim(Mid(InLine, INSTR(InLine, "=") + 1))
			
			Select Case Trim(Left(InLine, INSTR(InLine, "=") - 1))
				Case "configwords": ConfWords = VAL(TempData)
				Case "ram": ChipRam = VAL(TempData)
				Case "family": ChipFamily = VAL(TempData)
				Case "pins": ChipPins = Val(TempData)
				Case "psp": PSP = VAL(TempData)
				Case "maxaddress": MemSize = VAL(TempData)
				Case "prog": ChipProg = VAL(TempData)
				Case "hardwaremult":
					HMult = 0: If TempData = "y" Then HMult = -1
				Case "eeprom": ChipEEPROM = Val(TempData)
				Case "maxmhz": ChipMaxSpeed = Val(TempData)
				Case "intosc"
					'ChipIntOsc = Val(TempData)
					IntOscSpeeds = 0
					Do While InStr(TempData, ",") <> 0
						IntOscSpeeds += 1
						IntOscSpeed(IntOscSpeeds) = Val(TempData)
						TempData = Trim(Mid(TempData, InStr(TempData, ",") + 1))
					Loop
					If TempData <> "" Then
						If Val(TempData) <> 0 Then
							IntOscSpeeds += 1
							IntOscSpeed(IntOscSpeeds) = Val(TempData)
						End If
					End If
			End Select
		
		'Interrupts
		ElseIF ReadDataMode = "[interrupts]" AND INSTR(InLine, ":") <> 0 THEN
			IntCount += 1
			With Interrupts(IntCount)
				.EventName = Trim(Left(InLine, INSTR(InLine, ":") - 1))
				TDC = 0
				TempData = Trim(Mid(InLine, INSTR(InLine, ":") + 1))
				Do While Instr(TempData, ",") <> 0
					TDC += 1: TempList(TDC) = TRIM(Left(TempData, Instr(TempData, ",") - 1))
					TempData = Trim(Mid(TempData, Instr(TempData, ",") + 1))
				Loop
				If TempData <> "" Then
					TDC += 1: TempList(TDC) = TempData
				End If
				
				If ModePIC Then
					.Vector = ""
					.VectorLoc = 0
					.EnableBit = TempList(1)
					.FlagBit = TempList(2)
					.Handler = ""
					
				ElseIf ModeAVR Then
					.Vector = TempList(1)
					.VectorLoc = Val(TempList(2))
					.EnableBit = TempList(3)
					.FlagBit = TempList(4)
					.Handler = ""
					
				ElseIf ModeZ8 Then
					.Vector = TempList(1)
					.VectorLoc = Val(TempList(2))
					.EnableBit = TempList(3)
					.FlagBit = TempList(4)
					.Handler = ""
					
				End If
				
			End With
		
		'Registers
		ElseIf ReadDataMode = "[registers]" AND INSTR(InLine, ",") <> 0 THEN
			SVC = SVC + 1
			InLine = UCase(InLine)
			SysVars(SVC).Name = Trim(Left(InLine, INSTR(InLine, ",") - 1))
			SysVars(SVC).Location = Val(Trim(Mid(InLine, INSTR(InLine, ",") + 1)))
		
		'Bits
		' 1 = name 3 = parent
		' In file: name, parent, location
		ElseIf ReadDataMode = "[bits]" AND INSTR(InLine, ",") <> 0 THEN
			SVBC = SVBC + 1
			InLine = UCase(InLine)
			SysVarBits(SVBC).Name = Trim(Left(InLine, INSTR(InLine, ",") - 1))
			TempData = Trim(Mid(InLine, INSTR(InLine, ",") + 1))
			SysVarBits(SVBC).Parent = Trim(Left(TempData, INSTR(TempData, ",") - 1))
			SysVarBits(SVBC).Location = Val(Trim(Mid(TempData, INSTR(TempData, ",") + 1)))
		
		'FreeRAM
		ElseIf ReadDataMode = "[freeram]" AND INSTR(InLine, ":") <> 0 THEN
			MRC = MRC + 1
			MemRanges(MRC) = InLine
		
		'No bank RAM
		ElseIf ReadDataMode = "[nobankram]" And InStr(InLine, ":") <> 0 Then
			NoBankLocs += 1
			With NoBankLoc(NoBankLocs)
				.StartLoc = Val("&H" + Left(InLine, InStr(InLine, ":") - 1))
				.EndLoc = Val("&H" + Mid(InLine, InStr(InLine, ":") + 1))
			End With
			
		'Pinout data
		ElseIf Left(ReadDataMode, 6) = "[pins-" Then
			'Get first pin function and data direction from line
			TempData = Mid(InLine, InStr(InLine, ",") + 1)
			If InStr(TempData, ",") <> 0 Then
				TempData = Trim(Left(TempData, InStr(TempData, ",") - 1))
			End If
			'Split pin name and pin direction
			If InStr(TempData, "(") <> 0 Then
				PinName = Left(TempData, InStr(TempData, "(") - 1)
				PinDir = Mid(TempData, InStr(TempData, "(") + 1)
				If InStr(PinDir, ")") <> 0 Then
					PinDir = Left(PinDir, InStr(PinDir, ")") - 1)
				End If
			Else
				PinName = TempData
				PinDir = "P"
			End If
			
			If IsIOPinName(PinName) Then
				If Left(PinName, 2) = "gp" Then
					PinName = "GPIO." + Mid(PinName, 3)
				Else
					PinName = "PORT" + Mid(PinName, 2, 1) + "." + Mid(PinName, 3, 1)
				End If
				PinDirData = GetPinDirection(PinName)
				If PinDirData <> 0 Then
					PinDirData -> AllowedDirections = PinDir
				End If
			End If
			
		'Config
		ElseIF ReadDataMode = "[config]" THEN
			InLine = UCase(InLine)
			If INSTR(InLine, ":") <> 0 THEN InLine = Left(InLine, INSTR(InLine, ":") - 1)
			IF INSTR(InLine, ",") <> 0 THEN
				COC = COC + 1
				With ConfigOps(COC)
					.Op = Trim(Left(InLine, INSTR(InLine, ",") - 1))
					InLine = Trim(Mid(InLine, INSTR(InLine, ",") + 1))
					.Loc = VAL(InLine)
					IF INSTR(InLine, ",") <> 0 THEN
						.Val = VAL(Trim(Mid(InLine, INSTR(InLine, ",") + 1)))
					END IF
				End With
			END IF
			
		'Config options
		ElseIf ReadDataMode = "[configops]" Then
			InLine = UCase(InLine)
			If InStr(InLine, "=") <> 0 Then
				'Create new option
				ThisSetting = Callocate(SizeOf(ConfigSetting))
				LinkedListInsert(ConfigSettings, ThisSetting)
				ThisSetting->Options = LinkedListCreate
				CurrLoc = ThisSetting->Options
				
				With (*ThisSetting)
					'Get option name
					.Name = Trim(Left(InLine, InStr(InLine, "=") - 1))
					
					'Get option values
					TempData = Trim(Mid(InLine, InStr(InLine, "=") + 1))
					Do While InStr(TempData, ",") <> 0
						CurrLoc = LinkedListInsert(CurrLoc, Trim(Left(TempData, InStr(TempData, ",") - 1)))
						TempData = Trim(Mid(TempData, InStr(TempData, ",") + 1))
					Loop
					If TempData <> "" Then
						CurrLoc = LinkedListInsert(CurrLoc, TempData)
					End If
					
				End With
			End If
			
		'Default config settings (18 only)
		ElseIF ReadDataMode = "[asmconfig]" THEN
			DCOC = DCOC + 1
			DefCONFIG(DCOC) = UCase(InLine)
		
		'Config masks (18 only)
		ElseIF ReadDataMode = "[configmask]" THEN
			ConfWords += 1
			ConfigMask(ConfWords) = VAL(InLine)
			
		END IF
		
		ReadNextChipInfoLine:
	LOOP
	CLOSE
	
	'Misc initialisation
	If ModePIC Then
		'Calculate the number of high PC bits
		PCUpper = 0
		IF ChipFamily = 12 Or ChipFamily = 14 THEN 
			IF ChipProg > 2048 THEN PCUpper = 1
			IF ChipProg > 4096 THEN PCUpper = 2
		END If
		
		'If there are no bank free ram locations defined, guess them
		'Typically expect 0:F on 12 bit, 70:7F on 14, and 0:5F on 16
		If NoBankLocs = 0 Then
			Select Case ChipFamily
				Case 12:
					NoBankLocs = 1
					With NoBankLoc(1)
						.StartLoc = 0
						.EndLoc = &HF
					End With
				Case 14, 15:
					NoBankLocs = 1
					With NoBankLoc(1)
						.StartLoc = &H70
						.EndLoc = &H7F
					End With
				Case 16:
					NoBankLocs = 2
					With NoBankLoc(1)
						.StartLoc = 0
						.EndLoc = &H5F
					End With
					'Access RAM for SFRs
					With NoBankLoc(2)
						.StartLoc = &HF60
						.EndLoc = &HFFF
					End With
			End Select
			
			'If on an 18F and there is only one set of non-banked locations, add SFR
			If ChipFamily = 16 And NoBankLocs = 1 Then
				NoBankLocs = 2
				With NoBankLoc(2)
					.StartLoc = NoBankLoc(1).StartLoc + &HF01
					.EndLoc = &HFFF
				End With
			End If
		End If
		
	ElseIf ModeAVR Then
		
	End If
	
	'Load assembly commands
	
	#IFDEF __FB_LINUX__
		ChipDataFile = ID + "/chipdata/core" + Str(ChipFamily) + ".dat"
	#ELSE
		ChipDataFile = ID + "\chipdata\core" + Str(ChipFamily) + ".dat"
	#ENDIF
	If OPEN(ChipDataFile For INPUT AS #1) <> 0 Then
		TempData = Message("MissingCoreFile")
		Replace TempData, "%core%", ChipDataFile
		Replace TempData, "%chip%", ChipName
		LogError TempData
		WriteErrorLog
		END
	End If
	
	DO WHILE NOT EOF(1)
		LINE INPUT #1, InLine
		InLine = Trim(UCase(InLine))
		IF InLine <> "" AND Left(InLine, 1) <> "'" THEN
			DO WHILE INSTR(InLine, Chr(9)) <> 0: Replace InLine, Chr(9), " ": LOOP
			
			ASMCC += 1
			With AsmCommands(ASMCC)
				
				'Get syntax and number of words
				.Syntax = Trim(Left(InLine, INSTR(InLine, ";") - 1))
				InLine = Mid(InLine, INSTR(InLine, ";") + 1)
				.Words = VAL(Trim(Left(InLine, INSTR(InLine, ";") - 1)))
				InLine = Mid(InLine, INSTR(InLine, ";") + 1)
				
				'Get binary instruction words
				CW = 0
				Do While INSTR(InLine, ";") <> 0
					CW += 1
					.Word(CW) = TRIM(Left(InLine, INSTR(InLine, ";") - 1))
					InLine = Trim(Mid(InLine, INSTR(InLine, ";") + 1))
				Loop
				If InLine <> "" Then
					CW += 1
					.Word(CW) = InLine
				End If
				
				'Get command name
				.Cmd = Trim(LCase(.Syntax))
				If INSTR(.Cmd, " ") <> 0 THEN .Cmd = Trim(Left(.Cmd, INSTR(.Cmd, " ") - 1))
				
				'Get parameters
				Dim As String ParamName
				ParamName = .Syntax
				IF INSTR(ParamName, " ") = 0 Then
					ParamName = ""
				Else
					ParamName = Trim(Mid(ParamName, INSTR(ParamName, " ") + 1))
				End If
				GetTokens (ParamName, .Param(), .Params, ",")
				
			End With
			
		END IF
	Loop
	CLOSE
	
	'Check for presence of High FSR bit
	HighFSR = 0
	If ChipFamily = 12 Or ChipFamily = 14 Then
		FOR PD = 1 TO SVBC
			If SysVarBits(PD).Name = "IRP" Then
				HighFSR = -1
				Exit For
			End If
		Next
	End If
	
End Sub

Sub ReadOptions(OptionsIn As String)
	'Process #option statements
	
	Dim As String OptionElement(100)
	Dim As Integer OptionElements, CurrElement
	
	'Set defaults
	Bootloader = 0
	
	'Get settings
	GetTokens(OptionsIn, OptionElement(), OptionElements)
	For CurrElement = 1 To OptionElements
		'Get bootloader setting
		If OptionElement(CurrElement) = "BOOTLOADER" Then
			If OptionElements > CurrElement Then
				If IsConst(OptionElement(CurrElement + 1)) Then
					Bootloader = MakeDec(OptionElement(CurrElement + 1))
				End If
			End If
		End If
		
	Next
	
End Sub

Sub RecordSubCall(CompSub As SubType Pointer, CalledSub As SubType Pointer)
	
	Dim FindCall As LinkedListElement Pointer
	
	If CompSub = 0 Then Exit Sub
	If CalledSub = 0 Then Exit Sub
	
	With *CompSub
		
		'Check for sub in list already
		FindCall = .CallList->Next
		Do While FindCall <> 0
			If FindCall->MetaData = CalledSub Then
				FindCall->NumVal += 1
				Exit Sub
			End If
			FindCall = FindCall->Next
		Loop
		
		'Sub not in list, add it
		'.Call(.Calls) = CalledSub
		FindCall = LinkedListInsert(.CallList, CalledSub)
		FindCall->NumVal = 1
	End With
	
End Sub

Function ReplaceFnNames(InName As String) As String
	'Replace tokens in sub names with actual names
	
	Dim As String OutName
	Dim As Integer Temp
	OutName = InName
	
	'Function name
	Do While Instr(OutName, Chr(31)) <> 0
		Temp = VAL(MID(OutName, Instr(OutName, Chr(31)) + 1))
		Replace OutName, Chr(31) + Str(Temp) + Chr(31), Subroutine(Temp)->Name
	Loop
	
	'Function name (used differently to 31)
	Do While Instr(OutName, Chr(30)) <> 0
		Temp = VAL(MID(OutName, Instr(OutName, Chr(30)) + 1))
		Replace OutName, Chr(30) + Str(Temp) + Chr(30), Subroutine(Temp)->Name
	Loop
	
	'Miscellaneous item. Used for array names, may be used for other things
	Do While Instr(OutName, Chr(29)) <> 0
		Temp = VAL(MID(OutName, Instr(OutName, Chr(29)) + 1))
		Replace OutName, Chr(29) + Str(Temp) + Chr(29), PreserveCode(Temp)
	Loop
	
	Return OutName
End Function

Function RequestSub(Requester As SubType Pointer, SubNameIn As String, SubSigIn As String = "") As Integer
	'Will return index of sub in Subroutine
	'Requester is used to log which sub calls which
	'If Requester is 0, it will be set to main sub
	'If sub not found, will return -1
	
	Dim As Integer CurrSub, BestMatchPos, BestMatch, ThisMatch, InReqList
	Dim As String SubName, SubSig
	Dim As LinkedListElement Pointer ReqListPos, ReqListData
	
	SubName = UCase(Trim(SubNameIn))
	BestMatchPos = -1
	BestMatch = -1
	
	If Requester = 0 Then
		Requester = Subroutine(0)
	End If
	
	'If no sub being called, exit before recording attempt
	If SubName = "" Then
		Return -1
	End If
	
	For CurrSub = 1 To SBC
		If UCase(Trim(Subroutine(CurrSub)->Name)) = SubName Then
			If Subroutine(CurrSub)->Overloaded Then
				ThisMatch = SubSigMatch(GetSubSig(*Subroutine(CurrSub)), SubSigIn)
				If ThisMatch > BestMatch Then
					BestMatch = ThisMatch
					BestMatchPos = CurrSub
				End If
			Else
				Subroutine(CurrSub)->Required = -1
				RecordSubCall(Requester, Subroutine(CurrSub))
				
				Return CurrSub
			End If
		End If
	Next
	
	If BestMatch <> -1 Then
		Subroutine(BestMatchPos)->Required = -1
		RecordSubCall(Requester, Subroutine(BestMatchPos))
		Return BestMatchPos
	End If
	
	'Record failed attempted call
	'May be able to grant request later
	'Only add to list if not already in it
	ReqListPos = AttemptedCallList->Next
	InReqList = 0
	Do While ReqListPos <> 0
		With *ReqListPos
			If .MetaData <> 0 Then
				ReqListData = .MetaData
				'Put sub name in .Value, sub sig in .MetaData->Value, caller in .MetaData->MetaData
				If ReqListData->MetaData = Requester Then
					If LCase(.Value) = LCase(SubName) And LCase(ReqListData->Value) = LCase(SubSigIn) Then
						InReqList = -1
						Exit Do
					End If
				End If
			End If
		End With
		ReqListPos = ReqListPos->Next
	Loop
	
	'Not in requested list, add
	If Not InReqList Then
		ReqListPos = LinkedListInsert(AttemptedCallList, SubName)
		ReqListData = LinkedListCreate
		ReqListPos->MetaData = ReqListData
		ReqListData->Value = SubSigIn
		ReqListData->MetaData = Requester
	End If
	
	Return -1
End Function

Sub RetrySubRequests
	'Check AttemptedCallList for calls that could not be recorded
	'Attempt to record them
	
	Dim As LinkedListElement Pointer ReqListPos, ReqListData
	
	ReqListPos = AttemptedCallList->Next
	Do While ReqListPos <> 0
		
		If ReqListPos->MetaData <> 0 Then
			ReqListData = ReqListPos->MetaData
			RequestSub(ReqListData->MetaData, ReqListPos->Value, ReqListData->Value)
		End If
		
		ReqListPos = ReqListPos->Next
	Loop
	
End Sub

Sub SetCalcTempType (CalcVar As String, NewType As String)
	
	'Don't allow calc vars to be bit
	If UCase(NewType) = "BIT" Then NewType = "BYTE"
	
	Select Case UCASE(RIGHT(TRIM(CalcVar), 1))
		Case "A": CalcTempType(1) = UCASE(TRIM(NewType))
		Case "B": CalcTempType(2) = UCASE(TRIM(NewType))
		Case "X": CalcTempType(3) = UCASE(TRIM(NewType))
	End Select
	
End Sub

Function SetStringPointers (V1 As String, V2 As String, CurrSub As SubType Pointer, Origin As String) As LinkedListElement Pointer
	
	Dim As String Temp
	
	'Prepare output list
	Dim As LinkedListElement Pointer OutList, CurrLine
	OutList = LinkedListCreate
	CurrLine = OutList
	
	'Remove $ from end of var name
	IF Right(V1, 1) = "$" Then V1 = Left(V1, LEN(V1) - 1)
	IF Right(V2, 1) = "$" Then V2 = Left(V2, LEN(V2) - 1)
	
	'Check input
	If INSTR(UCase(V1), ";STRING") <> 0 Then
		LogError Message("BadStringConst"), Origin
		Return OutList
	End If
	'If V2 is a string, put it into a temp array
	If INSTR(UCase(V2), ";STRING") <> 0 Then
		AddVar("SysTempArray", "STRING", 20, 0, "REAL", Origin)
		'CSC += 1: TempData(CSC) = "SYSTEMPARRAY()=" + V2
		CurrLine = LinkedListInsert(CurrLine, "SYSTEMPARRAY()=" + V2)
		V2 = "SYSTEMPARRAY"
	End If
	
	'Add code to set pointers
	'Load V1 into 0, V2 into 1
	CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(V1, 0, CurrSub, Origin))
	CurrLine = LinkedListInsertList(CurrLine, GenerateArrayPointerSet(V2, 1, CurrSub, Origin))
	
	Return OutList
End Function

SUB ShowBlock (BlockIn As String)
	
	Dim As String Block, Temp, LineIn, DispLine
	Dim As Integer InBlock
	Block = UCase(Trim(BlockIn))
	
	#IFDEF __FB_LINUX__
		OPEN ID + "/messages.dat" FOR INPUT AS #9
	#ELSE
		OPEN ID + "\messages.dat" FOR INPUT AS #9
	#ENDIF
	
	InBlock = 0
	DO WHILE NOT EOF(9)
		LINE INPUT #9, LineIn
		DispLine = LineIn
		LineIn = UCase(Trim(LineIn))
		IF LineIn = "BLOCK " + Block THEN
			InBlock = -1
		ElseIf InBlock Then
			If LineIn = "END BLOCK" THEN
				Exit Do
			Else
				Do While INSTR(UCase(DispLine), "%VERSION%") <> 0: Replace DispLine, "%VERSION%", Version: Loop
				Do While INSTR(UCase(DispLine), "%FILENAME%") <> 0: Replace DispLine, "%FILENAME%", FI: Loop
				Do While INSTR(UCase(DispLine), "%OUTPUTFILE%") <> 0: Replace DispLine, "%OUTPUTFILE%", OFI: Loop
				PRINT DispLine
			End If
		End If
	LOOP
	CLOSE #9
	
END SUB

SUB SplitLines (CompSub As SubType Pointer)
	
	Dim As String Temp, LastHalf
	Dim As LinkedListElement Pointer CurrLine
	
	'Search main program
	CurrLine = CompSub->CodeStart->Next
	Do While CurrLine <> 0
		IF INSTR(CurrLine->Value, ":") <> 0 AND Right(CurrLine->Value, 1) <> ":" THEN
			Temp = CurrLine->Value
			LastHalf = LTrim(Mid(Temp, INSTR(Temp, ":") + 1))
			CurrLine->Value = RTrim(Left(Temp, INSTR(Temp, ":") - 1))
			LinkedListInsert(CurrLine, LastHalf)
		Else
			CurrLine = CurrLine->Next
		End If
	Loop
	
END SUB

Function TempRemove(Removed As String) As String
	'Temporarily remove something from a line. Can be replaced later with ReplaceFnNames
	
	'Record removed item
	PCC += 1: PreserveCode(PCC) = Removed
	
	Return Chr(29) + Str(PCC) + Chr(29)
End Function

Sub TidyProgram
	Dim As Integer CurrSub
	
	For CurrSub = 0 To SBC
		With *Subroutine(CurrSub)
			If .Required Then
				TidySubroutine(Subroutine(CurrSub))
			End If
		End With
	Next
End Sub

Sub TidySubroutine(CompSub As SubType Pointer)
		
	'Fix function calls
	FixFunctions(CompSub)
	
	'Enable/disable interrupts as needed
	CompileIntOnOff (CompSub)
	
	'Set Bank
	AddBankCommands(CompSub)
	
	'Perform some simple ASM optimisations
	AsmOptimiser(CompSub)
	
	'Add banking mode to 18F commands
	If ChipFamily = 16 THEN
		Add18FBanks(CompSub)
	End If
	
	'Tidy up IFs
	OptimiseIf (CompSub)
	
End Sub

Function TranslateFile(InFile As String) As String
	'Translates file to GCBASIC format
	
	Dim As String OutFile, ConvFile, CurrType, ConvExe, ConvParams, PathDiv
	Dim As String ConvDir, Temp
	Dim As Integer Converted, Converter, CurrForm, ConvertRequired
	Dim As Double InDate, OutDate
	OutFile = InFile
	
	'Get converter directory
	#Ifdef __FB_LINUX__
		ConvDir = ID + "/converters"
		PathDiv = "/"
	#ELSE
		ConvDir = ID + "\converters"
		PathDiv = "\"
	#EndIf
	
	'Might need to do multiple conversions
	Do
		Converted = 0
		
		'Find converter for current type
		CurrType = Trim(LCase(Mid(OutFile, InStrRev(OutFile, ".") + 1)))
		Converter = -1
		For Converter = 1 To FileConverters
			With FileConverter(Converter)
				'Check all formats for current converter
				For CurrForm = 1 To .InFormats
					If .InFormat(CurrForm) = CurrType Then
						'Have found a converter
						'Check for converted file
						ConvFile = ReplaceToolVariables("%filename%", .OutFormat, OutFile)
						ConvertRequired = 0
						'If no converted file, need to convert
						If Dir(ConvFile) = "" Then
							ConvertRequired = -1
						'If converted file older than source file, need to convert
						Else
							InDate = FileDateTime(OutFile)
							OutDate = FileDateTime(ConvFile)
							If InDate > OutDate Then ConvertRequired = -1
						End If
						
						If VBS = 1 Then
							Temp = Message("Converting")
							Replace Temp, "%infile%", OutFile
							Print Spc(10); Temp;
						End If
						
						'If convert needed, convert
						If ConvertRequired Then
							
							ConvExe = AddFullPath(.ExeName, ConvDir)
							If Exec(ConvExe, OutFile) <> -1 Then
								If VBS = 1 Then Print Message("Success")
								OutFile = ConvFile
								Converted = -1
							Else
								Print Message("NoConverter")
							End If
						Else
							If VBS = 1 Then Print Message("ConvertedAlready")
							OutFile = ConvFile
							Converted = -1
						End If
						
						GoTo CheckNextFormat
					End If
				Next
			End With
		Next
		
		CheckNextFormat:
	Loop While Converted
	
	Return OutFile
End Function

Sub ValueChanged(VarName As String, VarValue As String)
	
	If VarName <> "" Then Exit Sub
	IF Left(UCase(VarName), 7) <> "SYSTEMP" Then Exit Sub
	
	CV = VAL(Mid(Trim(VarName), 8))
	
	CalcVars(CV).CurrentValue = VarValue
End Sub

Sub WriteAssembly
	Dim As String Temp, VarDecLine
	Dim As Integer PD, AddSFR, FindSREG
	Dim As LinkedListElement Pointer CurrLine
	
	'Write .ASM program
	OPEN OFI FOR OUTPUT AS #1
	
	PRINT #1, ";Program compiled by Great Cow BASIC (" + Version + ")"
	Print #1, ";Need help? See the GCBASIC forums at http://sourceforge.net/projects/gcbasic/forums,"
	Print #1, ";check the documentation or email w_cholmondeley at users dot sourceforge dot net."
	Print #1, ""
	PRINT #1, Star80
	PRINT #1, ""
	
	If ModePIC Then
		PRINT #1, ";Set up the assembler options (Chip type, clock source, other bits and pieces)"
		PRINT #1, " LIST p=" + ChipName + ", r=DEC"
		'Workaround for 16C5x chips
		If Len(ChipName) = 5 And Left(ChipName, 2) = "16" And Mid(ChipName, 4, 1) = "5" Then
			PRINT #1, "#include <P16" + Mid(ChipName, 3, 1) + "5X.inc>"
		Else
			PRINT #1, "#include <P" + ChipName + ".inc>"
		End If
		
		CurrLine = ChipConfigCode->CodeList->Next
		Do While CurrLine <> 0
			Print #1, CurrLine->Value
			CurrLine = CurrLine->Next
		Loop 
	
	ElseIf ModeAVR Then
		Temp = LCase(ChipName)
		If INSTR(Temp, "usb") <> 0 Then
			Temp = MID(Temp, INSTR(Temp, "usb"))
		ElseIf INSTR(Temp, "can") <> 0 Then
			Temp = MID(Temp, INSTR(Temp, "can"))
		ElseIf Left(Temp, 4) = "mega" THEN
			Temp = "m" + Mid(Temp, 5)
		ElseIf Left(Temp, 4) = "tiny" THEN
			Temp = "tn" + Mid(Temp, 5)
		ElseIf INSTR(Temp, "s") <> 0 Then
			Temp = Mid(Temp, INSTR(Temp, "s") + 1)
		End If
		PRINT #1, ";Assembler header file"
		PRINT #1, ".INCLUDE " + Chr(34) + Temp + "def.inc" + Chr(34)
		Print #1, ""
		Print #1, ";SREG bit names (for AVR Assembler compatibility, GCBASIC uses different names)"
		For FindSREG = 1 To SVBC
			If UCASE(SysVarBits(FindSREG).Parent) = "SREG" Then
				Print #1, "#define " + SysVarBits(FindSREG).Name + " " + Str(SysVarBits(FindSREG).Location) 
			End If
		Next
	
	ElseIf ModeZ8 Then
		PRINT #1, ";Set up the assembler options (Chip type, clock source, other bits and pieces)"
		PRINT #1, AsmTidy(" CPU = " + ChipName)
		Print #1, ""
		
		'List registers and bits, no .inc file available for assembler
		Print #1, ";Special function registers"
		For PD = 1 To SVC
			
			AddSFR = -1
			Temp = UCase(SysVars(PD).Name)
			If Left(Temp, 1) = "R" And IsConst(Mid(Temp, 2)) Then AddSFR = 0
			If Temp = "SPH" Then AddSFR = 0
			If Temp = "SPL" Then AddSFR = 0
			If Temp = "FLAGS" Then AddSFR = 0
			If Temp = "RP" Then AddSFR = 0
			
			If AddSFR Then Print #1, AsmTidy(SysVars(PD).Name + " EQU " + Str(SysVars(PD).Location)) 
		Next
		Print #1, ""
		Print #1, ";Ram end"
		Print #1, AsmTidy("RAMEND EQU " + Mid(MemRanges(MRC), InStr(MemRanges(MRC), ":") + 1) + "h") 
		Print #1, ""
		Print #1, ";Special function register bits"
		'For PD = 1 To SVC
		'	Print #1, AsmTidy(SysVars(PD, 1) + " EQU " + SysVars(PD, 2)) 
		'Next	
		
		Print #1, AsmTidy(" ORG 0")
		Temp = " DB (%FF"
		If OutConfig(1) <> "" Then Temp += "&" + OutConfig(1)
		Temp += "), (%FF"
		If OutConfig(2) <> "" Then Temp += "&" + OutConfig(2)
		Print #1, Temp + ")"
		
	End If
	PRINT #1, ""
	
	IF FVLC > 0 THEN
		PRINT #1, Star80
		PRINT #1, ""
		PRINT #1, ";Set aside memory locations for variables"
		FOR PD = 1 TO FVLC
			With FinalVarList(PD)
				If ModePIC Or ModeZ8 Then
					VarDecLine = .Name + " EQU " + .Value
				ElseIf ModeAVR Then
					VarDecLine = ".EQU " + .Name + "=" + .Value
				Else
					VarDecLine = ";Write variable declaring code!"
				End If
			End With
			PRINT #1, AsmTidy(VarDecLine)
		NEXT
		PRINT #1, ""
	END IF
	PRINT #1, Star80
	PRINT #1, ""
	
	'Registers
	If ModeAVR Then
		Print #1, AsmTidy(";Register variables")
		FOR PD = 1 TO FRLC
			With FinalRegList(PD)
				If ModePIC Or ModeZ8 Then
					VarDecLine = .Name + " EQU " + .Value
				ElseIf ModeAVR Then
					VarDecLine = ".DEF " + .Name + "=" + .Value
				Else
					VarDecLine = ";Write variable declaring code!"
				End If
			End With
			PRINT #1, AsmTidy(VarDecLine)
		NEXT
		PRINT #1, ""
		PRINT #1, Star80
		PRINT #1, ""
		
	End If
	
	'Aliases
	IF FALC > 0 THEN
		PRINT #1, AsmTidy(";Alias variables")
		FOR PD = 1 TO FALC
			With FinalAliasList(PD)
				If ModePIC Or ModeZ8 Then
					VarDecLine = .Name + " EQU " + .Value
				ElseIf ModeAVR Then
					VarDecLine = "#define " + .Name + " " + .Value
				Else
					VarDecLine = ";Write variable declaring code!"
				End If
			End With
			PRINT #1, AsmTidy(VarDecLine)
		NEXT
		PRINT #1, ""
		PRINT #1, Star80
		PRINT #1, ""
	END IF
	
	'Program code
	CurrLine = CompilerOutput->CodeList->Next
	Do While CurrLine <> 0
		PRINT #1, AsmTidy(CurrLine->Value)
		CurrLine = CurrLine->Next
	Loop
	PRINT #1, ""
	If ModePIC Then PRINT #1, " END"
	CLOSE #1
End Sub

Sub WriteCompilationReport
	'Create a report on compilation process
	
	Dim As String RF, ReportFileName, OutData, Temp, CallText
	Dim As Integer F, CurrSubNo, CurrBank, CurrBankLoc
	Dim As SubType Pointer CurrSub, CalledSub
	Dim As LinkedListElement Pointer ListItem
	
	'Save typing
	RF = CompReportFormat
	
	'Check requested format: valid options are HTML or plain text
	If RF <> "html" And RF <> "text" Then Exit Sub
	
	'Open file for output
	If RF = "html" Then
		ReportFileName = ReplaceToolVariables("%filename%", "html")
	ElseIf RF = "text" Then
		ReportFileName = ReplaceToolVariables("%filename%", "report.txt")
	End If
	F = FreeFile
	Open ReportFileName For Output As #F
	
	'Write header of file
	If RF = "html" Then
		Print #F, "<!DOCTYPE html>"
		Print #F, "<html>"
		Print #F, "<head>"
		Print #F, "<title>" + Message("CRTitle") + "</title>"
		Print #F, "</head>"
		Print #F, "<body>"
		Print #F, "<h1>" + Message("CRTitle") + "</h1>"
	ElseIf RF = "text" Then
		Print #F, Message("CRTitle")
		Print #F, ""
	End If
	
	'Write compiler information
	If RF = "html" Then
		Print #F, "<p>" + Message("CRVersion") + ": " + Version + "</p>"
	ElseIf RF = "text" Then
		Print #F, Message("CRVersion") + ": " + Version
		Print #F, ""
		Print #F, Star80
		Print #F, ""
	End If
	
	'Write chip resource usage information
	Dim As String UsedProgram, UsedRAM
	UsedProgram = Message("UsedProgram")
	Replace UsedProgram, "%used%", Str(StatsUsedProgram)
	Replace UsedProgram, "%total%", Str(ChipProg)
	If ChipProg <> 0 Then UsedProgram += Format(StatsUsedProgram / ChipProg, " (###.##%)")
	UsedRAM = Message("UsedRAM")
	Replace UsedRAM, "%used%", Str(StatsUsedRam)
	Replace UsedRAM, "%total%", Str(ChipRAM)
	If ChipRAM <> 0 Then UsedRAM += Format(StatsUsedRAM / ChipRAM, " (###.##%)")
	
	If RF = "html" Then
		Print #F, "<h2>" + Message("ChipUsage") + "</h2>"
		Print #F, "<p>" + UsedProgram + "</p>"
		Print #F, "<p>" + UsedRAM + "</p>"
		
	ElseIf RF = "text" Then
		Print #F, Message("ChipUsage")
		Print #F, UsedProgram
		Print #F, UsedRAM
		Print #F, ""
		Print #F, Star80
		Print #F, ""
	End If
	
	'Write chip pin usage information
	
	'Write variable sizes/locations
	'Display variable name, type and location
	If RF = "html" Then
		Print #F, "<h2>" + Message("CRRAM") + "</h2>"
		Print #F, "<table border=""1"">"
	Else
		Print #F, Message("CRRAM")
	End If
	
	If RF = "html" Then
		Print #F, "</table>"
	ElseIf RF = "text" Then
		Print #F, Star80
		Print #F, ""
	End If
	
	'Write subroutine sizes/locations/calls
	If RF = "html" Then
		Print #F, "<h2>" + Message("CRSubInfo") + "</h2>"
		Print #F, "<table border=""1"">"
		OutData = "<tr><th>" + Message("CRName") + "</th><th>" + Message("CROriginalSize")
		OutData = OutData + "</th><th>" + Message("CRSize") + "</th><th>"
		If ModePIC And (ProgMemPages > 1) Then
			OutData = OutData + Message("CRPage") + "</th><th>"
		End If
		OutData = OutData + Message("CRCalls") + "</th></tr>"
		Print #F, OutData
		
	ElseIf RF = "text" Then
		Print #F, Message("CRSubInfo")
	End If
	For CurrSubNo = 0 To SBC
		CurrSub = Subroutine(CurrSubNo)
		
		If CurrSub->Required Then
			'Get call list
			CallText = ""
			ListItem = CurrSub->CallList->Next
			Do While ListItem <> 0
				CalledSub = ListItem->Metadata
				If CallText = "" Then
					CallText = CalledSub->Name + "(" + Str(ListItem->NumVal) + ")"
				Else
					CallText = CallText + ", " + CalledSub->Name + "(" + Str(ListItem->NumVal) + ")"
				End If
				ListItem = ListItem->Next
			Loop
			
			'Write information
			If RF = "html" Then
				OutData = "<tr><td>" + CurrSub->Name + "</td>"
				OutData += "<td>" + Str(CurrSub->OriginalLOC) + "</td>"
				OutData += "<td>" + Str(CurrSub->HexSize) + "</td>"
				If ModePIC And (ProgMemPages > 1) Then
					OutData += "<td>" + Str(CurrSub->DestPage) + "</td>"
				End If
				OutData += "<td>" + CallText + "</td></tr>"
				Print #F, OutData
				
			ElseIf RF = "text" Then
				Print #F, CurrSub->Name
				Print #F, Message("CROriginalSize") + ": " + Str(CurrSub->OriginalLOC)
				Print #F, Message("CRSize") + ": " + Str(CurrSub->HexSize)
				If ModePIC And (ProgMemPages > 1) Then
					Print #F, Message("CRPage") + ": " + Str(CurrSub->DestPage)
				End If
				If CallText <> "" Then Print #F, Message("CRCalls") + ": " + CallText
				Print #F, ""
			End If
		End If
	Next
		
	'Finish
	If RF = "html" Then
		Print #F, "</table>"
		Print #F, "</body>"
		Print #F, "</html>"
	ElseIf RF = "text" Then
		Print #F, Star80
		Print #F, ""
	End If
	Close #F
	
End Sub

SUB WriteErrorLog
	Dim As String Temp, MessageType, NewErr, ErrorFileName
	Dim As Integer CD, CS, PD, F1, L1, F2, L2, F, L, S, I, T
	Dim As Integer ERC, WRC, SortEnd
	
	'Set error log filename
	ErrorFileName = "Errors.txt"
	
	'Exit sub if no errors are present
	IF OutMessages = 0 Then
		Kill ErrorFileName
		EXIT Sub
	End If	
	
	'Sort error list
	'Print "Sort error list"
	SortEnd = OutMessages - 1
	If OutMessages = MAX_OUTPUT_MESSAGES Then SortEnd = OutMessages - 2
	Do
		CS = 0
		FOR PD = 1 TO SortEnd
			'Get file and line of two errors
			F1 = 0: L1 = 0: F2 = 0: L2 = 0
			IF INSTR(OutMessage(PD), ";?F") <> 0 THEN
				Temp = OutMessage(PD)
				F1 = VAL(Mid(Temp, INSTR(Temp, "F")+1))
				L1 = VAL(Mid(Temp, InStr(Temp, "L")+1))
			END IF
			IF INSTR(OutMessage(PD + 1), ";?F") <> 0 THEN
				Temp = OutMessage(PD + 1)
				F2 = VAL(Mid(Temp, INSTR(Temp, "F")+1))
				L2 = VAL(Mid(Temp, INSTR(Temp, "L")+1))
			END IF
			
			'Compare, swap if necessary
			IF (F1 > F2) Or (F1 = F2 AND L1 > L2) THEN
				Swap OutMessage(PD), OutMessage(PD + 1)
				CS = 1
			END IF
		NEXT
	Loop While CS = 1
	'Print "Finished sorting"
	
	'Generate file:line references
	FOR PD = 1 to OutMessages
		'Get type
		MessageType = Right(OutMessage(PD), 1)
		OutMessage(PD) = Left(OutMessage(PD), LEN(OutMessage(PD)) - 1)
		If MessageType = "W" Then MessageType = Message("TypeWarning"): WRC += 1
		If MessageType = "E" Then MessageType = Message("TypeError"): ERC += 1
		If LEN(MessageType) > 1 And UCase(LEFT(OutMessage(PD), 4)) <> "ASM:" Then
			MessageType = MessageType + ": "
		Else
			MessageType = ""
		End If
		
		Dim As String Me, Fi
		IF INSTR(OutMessage(PD), ";?F") <> 0 THEN
			Temp = OutMessage(PD)
			'Get values
			Replace Temp, "?", ""
			Me = Trim(Mid(Temp, INSTR(Temp, "?") + 1)) 'Message
			F = VAL(Mid(Temp, INSTR(Temp, "F") + 1)) 'File
			L = VAL(Mid(Temp, INSTR(Temp, "L") + 1)) 'Line
			S = VAL(Mid(Temp, INSTR(Temp, "S") + 1)) 'Subroutine
			I = VAL(Mid(Temp, INSTR(Temp, "I") + 1)) 'Subroutine line
			
			'If F and L = 0, location isn't known
			If F = 0 And L = 0 Then
				OutMessage(PD) = MessageType + OutMessage(PD)
				
			Else
				'Trim filename of path
				Fi = gcINC(F, 1)
				FOR T = LEN(Fi) to 1 step -1
					IF Mid(Fi, T, 1) = "\" THEN Fi = Mid(Fi, T + 1): Exit For
				NEXT
				'Prepare message
				If GCGB = 0 Then
					OutMessage(PD) = Fi + " (" + Str(L) + "): " + MessageType + Me
				Else
					'Use special error format for GCGB (or other IDE that breaks program into subs)
					OutMessage(PD) = Left(MessageType, 1) + "|" + Fi + "|" + Str(S) + "|" + Str(I) + "|" + Me
				End If
			End If
		Else
			OutMessage(PD) = MessageType + OutMessage(PD)
		END IF
		
	NEXT

	'Display message
	IF ERC = 1 THEN Temp = Message("Error")
	IF ERC > 1 THEN Temp = Message("Errors")
	IF ERC = 0 And WRC = 1 Then Temp = Message("Warning")
	IF ERC = 0 And WRC > 1 Then Temp = Message("Warnings")

	'If not being called by GCGB/other IDE, display errors
	IF GCGB = 0 THEN
		PRINT
		PRINT Temp
		PRINT
		FOR PD = 1 TO OutMessages
			Temp = OutMessage(PD)
			'Display errors in 77 columns neatly
			'77 chosen because it allows for the whole thing to fit in a Windows command
			'prompt, the screen is normally 80 columns but the scrollbar takes away 3.
ShowError:
			IF LEN(Temp) <= 77 THEN PRINT SPC(1); Temp
			IF LEN(Temp) > 77 THEN
				FOR T = 77 TO 1 STEP -1
					IF Mid(Temp, T, 1) = " " THEN
						PRINT SPC(1); Left(Temp, T)
						Temp = Mid(Temp, T + 1)
						GOTO ShowError
					END IF
				NEXT
			END IF
		NEXT
	END If
	
	'Write error log
	PRINT
	PRINT Message("ErrorLogged") + ErrorFileName + "."
	OPEN ErrorFileName FOR OUTPUT AS #1
	FOR PD = 1 TO OutMessages
		PRINT #1, OutMessage(PD)
	NEXT
	CLOSE
	PRINT
	
	'Pause if errors found and pause enabled
	IF PauseOnErr = 1 And ErrorsFound Then
		PRINT Message("AnyKey")
		DO WHILE INKEY = "": SLEEP 10: LOOP
	END IF
	
END SUB

Sub MergeSubroutines
	
	'Combine subroutines into single program
	Dim As Integer CurrSub, TotalProgSize, FitAttempts, SubsAdded, SubQueueCount, Temp
	Dim As Integer SortPos, MovePos, ThisSub, RequiredSubs, CurrPage, OldTotalProgSize
	Dim As Integer SubsFit, FirstPageFull, ForceFit, CurrPagePos, SortMore, CurrListPage
	Dim As AllocationOrderType AllocationOrder(MAX_PROG_PAGES)
	Dim As SubType Pointer CurrSubPtr
	Dim As LinkedListElement Pointer CurrLine
	Dim As String SubNameOut
	
	Dim As SubType Pointer IntSub
	Dim As Integer IntSubLoc
	
	'Set ForceFit if on AVR or 18F
	'OptimiseCalls and OptimiseIf may reduce the code enough to fit, let the assembler worry if it doesn't
	ForceFit = 0
	If ModeAVR Or ChipFamily = 16 Then
		ForceFit = -1
	End If
	
	Dim As Integer SubQueue(SBC + 1)
	SubQueueCount = 0
	
	'Generate call tree of program
	CreateCallTree
	
	'Get location of interrupt sub (if it exists)
	IntSub = 0
	IntSubLoc = LocationOfSub("Interrupt", "")
	If IntSubLoc <> 0 Then
		IntSub = Subroutine(IntSubLoc)
	End If 
	
	If ForceFit Then
		'Force subs to fit
		'Put them all on page 1, assembler can deal with the mess
		For Temp = 0 To SBC
			With *Subroutine(Temp)
				If .Required Then
					.LocationSet = -1
					.DestPage = 1
					
					'Sub size is used for information only on 18F/AVR
					CalcSubSize(Subroutine(Temp))
				End If
			End With
		Next
	
	Else
		'Don't force subs to fit
		'Try to put them all in place, if they don't fit show an error
		'This should be used on 10F/12F/16F PIC, not on AVR or 18F
		
		'Measure size of subs
		TotalProgSize = 0
		For CurrSub = 0 To SBC
			If Subroutine(CurrSub)->Required Then
				CalcSubSize(Subroutine(CurrSub))
				TotalProgSize += Subroutine(CurrSub)->HexSize
				'Clear location set flag as well
				Subroutine(CurrSub)->LocationSet = 0
			End If
		Next
		
		'Get list of subs sorted by size, excluding those forced to page 0
		'Copy list of locations
		RequiredSubs = 0
		For Temp = 0 To SBC
			If Subroutine(Temp)->Required Then
				If Not Subroutine(Temp)->FirstPage Then
					SubQueueCount += 1
					SubQueue(SubQueueCount) = Temp
				End If
				RequiredSubs += 1
			End If
		Next
		'Sort by size
		'(Use insertion sort)
		For SortPos = 1 To SubQueueCount
			ThisSub = SubQueue(SortPos)
			MovePos = SortPos - 1
			'Do While MovePos > 0 And Subroutine(ThisSub)->HexSize > Subroutine(SubQueue(MovePos))->HexSize
			Do While MovePos > 0 And Subroutine(ThisSub)->MaxHexSize > Subroutine(SubQueue(MovePos))->MaxHexSize
				SubQueue(MovePos + 1) = SubQueue(MovePos)
				MovePos -= 1
			Loop
			SubQueue(MovePos + 1) = ThisSub
		Next
		
		'Clear all pages
		For CurrPage = 1 To ProgMemPages
			ProgMemPage(CurrPage).CodeSize = 0
		Next
		
		'Try moving subs and counting sizes until everything stabilises
		FitAttempts = 0
		Do
			FitAttempts += 1
			SubsAdded = 0
			
			'Prepare subroutines for allocation
			For CurrSub = 0 To SBC
				'Clear location set flags on subs
				Subroutine(CurrSub)->LocationSet = 0
				'Clear call counts
				For CurrPage = 0 To ProgMemPages
					Subroutine(CurrSub)->CallsFromPage(CurrPage) = 0
				Next
			Next
			
			'Clear size counters on pages
			For CurrPage = 1 To ProgMemPages
				ProgMemPage(CurrPage).CodeSize = 0
			Next
			
			'Position subroutines
			'Page 0 subs
			FirstPageFull = 0
			For CurrSub = 0 To SBC
				If Subroutine(CurrSub)->Required And Subroutine(CurrSub)->FirstPage Then
					Subroutine(CurrSub)->DestPage = 0
					'Print "Placing "; Subroutine(CurrSub)->Name; " on first page"
					ProgMemPage(1).CodeSize += Subroutine(CurrSub)->MaxHexSize
					If ProgMemPage(1).CodeSize < ProgMemPage(1).MaxSize Then
						SubsAdded += 1
						Subroutine(CurrSub)->LocationSet = -1
						Subroutine(CurrSub)->DestPage = 1
						'Record calls to other subroutines
						UpdateOutgoingCalls Subroutine(CurrSub)
					Else
						'Once the first page is full, no point trying to locate anything else
						FirstPageFull = -1
						GoTo StopTryingToFit
					End If
				End If
			Next
			
			'Other subs
			For CurrSub = 1 To SubQueueCount
				CurrSubPtr = Subroutine(SubQueue(CurrSub))
				'Print "Placing "; CurrSubPtr->Name; " anywhere"
				'Decide which pages are better for this subroutine
				For CurrPage = 1 To ProgMemPages
					AllocationOrder(CurrPage).Page = CurrPage
					AllocationOrder(CurrPage).Calls = CurrSubPtr->CallsFromPage(CurrPage)
				Next
				SortMore = -1
				Do While SortMore
					SortMore = 0
					For CurrPage = 1 To ProgMemPages - 1
						If AllocationOrder(CurrPage).Calls < AllocationOrder(CurrPage + 1).Calls Then
							Swap AllocationOrder(CurrPage), AllocationOrder(CurrPage + 1)
							SortMore = -1
						End If
					Next
				Loop
				
				'Where will this sub fit?
				'Note 2/6/2013: Implemented allocation order, and a program went from 7748 to 7761 words
				'Disabled again, more testing needed to see if it produces an overall benefit
				For CurrPage = 1 To ProgMemPages
				'For CurrListPage = 1 To ProgMemPages
				'	CurrPage = AllocationOrder(CurrListPage).Page
					
					With ProgMemPage(CurrPage)
						If .MaxSize >= .CodeSize + CurrSubPtr->MaxHexSize Then
							'Print , "page "; CurrPage; ", size "; CurrSubPtr->HexSize; ", curr filled "; .CodeSize; "/"; .MaxSize
							'Found a location
							CurrSubPtr->LocationSet = -1
							CurrSubPtr->DestPage = CurrPage
							.CodeSize += CurrSubPtr->MaxHexSize
							SubsAdded += 1
							'Record calls to other subroutines
							UpdateOutgoingCalls Subroutine(CurrSub)
							GoTo LocateNextSub
						End If
					End With
				Next
				
				LocateNextSub:
			Next
			
			'Calc new sizes
			OldTotalProgSize = TotalProgSize
			TotalProgSize = 0
			For CurrSub = 0 To SBC
				If Subroutine(CurrSub)->Required Then
					CalcSubSize(Subroutine(CurrSub))
					TotalProgSize += Subroutine(CurrSub)->HexSize
				End If
			Next
			
			'If everything still fits, can exit
			SubsFit = -1
			For CurrPage = 0 To ProgMemPages
				ProgMemPage(CurrPage).CodeSize = 0
				For CurrSub = 0 To SBC
					With *Subroutine(CurrSub)
						If .Required And .DestPage = CurrPage Then
							If .LocationSet Then
								ProgMemPage(CurrPage).CodeSize += .HexSize
								If ProgMemPage(CurrPage).CodeSize > ProgMemPage(CurrPage).MaxSize Then
									SubsFit = 0
									GoTo UnableToFit
								End If
							Else
								SubsFit = 0
								GoTo UnableToFit
							End If
						End If
					End With
				Next
			Next
			UnableToFit:
			If SubsFit And SubsAdded = RequiredSubs Then Exit Do
			
			'Print "Fit attempt"; FitAttempts; ", page sizes ";
			'For CurrPage = 1 To ProgMemPages
			'	Print Str(CurrPage); ":"; Str(ProgMemPage(CurrPage).CodeSize); " ";
			'Next
			'Print
			
		Loop While FitAttempts < 20
		StopTryingToFit:
		
		'If not all subs have been added, show an error
		If Not SubsFit Then
			If FirstPageFull Then
				LogError Message("FirstPageFull")
			Else
				LogError Message("OutOfProgMem")
			End If
		End If
	End If
	
	'Get a list of subs sorted into alphabetical order
	'Copy list of locations
	SubQueueCount = 0
	For Temp = 0 To SBC
		If Subroutine(Temp)->Required Then
			SubQueueCount += 1
			SubQueue(SubQueueCount) = Temp
		End If
	Next
	'Sort by name
	'(Use insertion sort)
	For SortPos = 2 To SubQueueCount
		ThisSub = SubQueue(SortPos)
		MovePos = SortPos - 1
		Do While MovePos > 1 And Subroutine(ThisSub)->Name < Subroutine(SubQueue(MovePos))->Name
			SubQueue(MovePos + 1) = SubQueue(MovePos)
			MovePos -= 1
		Loop
		SubQueue(MovePos + 1) = ThisSub
	Next
	
	'Add jumps in required vector locations
	CurrLine = CompilerOutput->CodeEnd
	CurrLine = LinkedListInsertList(CurrLine, GenerateVectorCode)
	
	'If on page 0 on 10/12/16, and interrupt exists, add it
	If ModePIC Then
		If IntSub <> 0 And ChipFamily <> 16 Then
			
			'Alter calls in sub (Use long/short calls as needed)
			AddPageCommands(IntSub)
			'Add sub code
			CurrLine = LinkedListInsertList(CurrLine, IntSub->CodeStart)
			'Blank, stars, blank at end
			CurrLine = LinkedListInsert(CurrLine, "")
			CurrLine = LinkedListInsert(CurrLine, Star80)
			CurrLine = LinkedListInsert(CurrLine, "")
			
			'Mark as no longer required so it isn't added again
			IntSub->Required = 0
		End If
	End If
	
	'Add all subs to output code
	'Add subs that could be located
	For CurrPage = 1 To ProgMemPages
		
		CurrLine = LinkedListInsert(CurrLine, ";Start of program memory page " + Str(CurrPage - 1))
		If ModePIC Then
			If ChipFamily = 16 Or IntSub = 0 Or CurrPage > 1 Then
				CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(ProgMemPage(CurrPage).StartLoc))
				CurrPagePos = ProgMemPage(CurrPage).StartLoc
			Else
				'On 10/12/16, Interrupt must go at very start of program
				'Allow it space in page 0.
				CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(ProgMemPage(CurrPage).StartLoc + IntSub->HexSize))
				CurrPagePos = ProgMemPage(CurrPage).StartLoc + IntSub->HexSize
			End If
			
		ElseIf ModeAVR Then
			CurrLine = LinkedListInsert(CurrLine, ".ORG " + Str(ProgMemPage(CurrPage).StartLoc))
			CurrPagePos = ProgMemPage(CurrPage).StartLoc
		ElseIf ModeZ8 Then
			CurrLine = LinkedListInsert(CurrLine, " ORG " + Str(ProgMemPage(CurrPage).StartLoc))
			CurrPagePos = ProgMemPage(CurrPage).StartLoc
		End If
		
		For CurrSub = 1 To SubQueueCount
			CurrSubPtr = Subroutine(SubQueue(CurrSub))
			If CurrSubPtr->Required And CurrSubPtr->LocationSet And CurrSubPtr->DestPage = CurrPage Then
				'Need to put the sub here
				'Name of sub
				If SubQueue(CurrSub) = 0 Then
					SubNameOut = "BASPROGRAMSTART"
				Else
					SubNameOut = GetSubFullName(SubQueue(CurrSub))
				End If
				'Print "Placing " + SubNameOut + " at 0x" + Hex(CurrPagePos) + " (size:" + Str(CurrSubPtr->HexSize) + ")"
				CurrPagePos += CurrSubPtr->HexSize
				StatsUsedProgram += CurrSubPtr->HexSize
				
				If CurrSubPtr->Overloaded Then
					CurrLine = LinkedListInsert(CurrLine, ";Overloaded signature: " + GetSubSig(*CurrSubPtr))
				End If
				'CurrLine = LinkedListInsert(CurrLine, ";Subroutine size:" + Str(CurrSubPtr->HexSize) + " words")
				CurrLine = LinkedListInsert(CurrLine, SubNameOut + LabelEnd)
				
				'Alter calls in sub (Use long/short calls as needed)
				AddPageCommands(CurrSubPtr)
				'Add sub code
				CurrLine = LinkedListInsertList(CurrLine, CurrSubPtr->CodeStart)
				'Return
				If Not CurrSubPtr->NoReturn Then
					If ModePIC Then
						If ChipFamily = 12 Then
							CurrLine = LinkedListInsert(CurrLine, " retlw 0")
						Else
							CurrLine = LinkedListInsert(CurrLine, " return")
						End If
					ElseIf ModeAVR Then
						CurrLine = LinkedListInsert(CurrLine, " ret")
					ElseIf ModeZ8 Then
						CurrLine = LinkedListInsert(CurrLine, " ret")
					End If
				End If
				'Blank, stars, blank at end
				CurrLine = LinkedListInsert(CurrLine, "")
				CurrLine = LinkedListInsert(CurrLine, Star80)
				CurrLine = LinkedListInsert(CurrLine, "")
			End If
		Next
	Next
	
	'Add any subs that couldn't be located to end of file
	'User may be able to manually locate them
	Dim As Integer FirstUnsetSub
	FirstUnsetSub = -1
	'For CurrSub = 0 To SBC
	For CurrSub = 1 To SubQueueCount
		CurrSubPtr = Subroutine(SubQueue(CurrSub))
		If CurrSubPtr->Required And Not CurrSubPtr->LocationSet Then
			If FirstUnsetSub Then
				CurrLine = LinkedListInsert(CurrLine, ";Subs that could not be automatically placed")
				CurrLine = LinkedListInsert(CurrLine, ";It may be possible to manually locate them, but you should upgrade to a larger chip if possible")
				CurrLine = LinkedListInsert(CurrLine, "")
				FirstUnsetSub = 0
			End If
			'Need to put the sub here
			'Name of sub
			If SubQueue(CurrSub) = 0 Then
				SubNameOut = "BASPROGRAMSTART"
			Else
				SubNameOut = GetSubFullName(SubQueue(CurrSub))
			End If
			CurrLine = LinkedListInsert(CurrLine, SubNameOut + LabelEnd)
			CurrLine = LinkedListInsert(CurrLine, ";This sub size:" + Str(CurrSubPtr->HexSize))
			StatsUsedProgram += CurrSubPtr->HexSize 
			'Add sub code
			CurrLine = LinkedListInsertList(CurrLine, CurrSubPtr->CodeStart)
			'Return
			If Not CurrSubPtr->NoReturn Then
				If ModePIC Then
					CurrLine = LinkedListInsert(CurrLine, " return")
				ElseIf ModeAVR Then
					CurrLine = LinkedListInsert(CurrLine, " ret")
				ElseIf ModeZ8 Then
					CurrLine = LinkedListInsert(CurrLine, " ret")
				End If
			End If
			'Blank, stars, blank at end
			CurrLine = LinkedListInsert(CurrLine, "")
			CurrLine = LinkedListInsert(CurrLine, Star80)
			CurrLine = LinkedListInsert(CurrLine, "")
		End If
	Next
	
	'Add EEPROM data tables
	Dim As Integer EPDataHeader, EPDataLoc, CurrEPTable, CurrEPItem
	Dim As String EPTempData
	EPDataHeader = 0
	IF DataTables > 0 Then
		For CurrEPTable = 1 TO DataTables
			If DataTable(CurrEPTable).Used And DataTable(CurrEPTable).StoreLoc = 1 Then
				
				'Add header (if not added)
				If Not EPDataHeader Then
					CurrLine = LinkedListInsert(CurrLine, Star80)
					CurrLine = LinkedListInsert(CurrLine, "")
					CurrLine = LinkedListInsert(CurrLine, "; Data Lookup Tables (data memory)")
					If ChipFamily = 12 Or ChipFamily = 14 Then
						CurrLine = LinkedListInsert(CurrLine, " ORG 0x2100")
					ElseIf ChipFamily = 15 Then
						CurrLine = LinkedListInsert(CurrLine, " ORG 0xF000")
					ElseIf ChipFamily = 16 Then
						CurrLine = LinkedListInsert(CurrLine, " ORG 0xF00000")
					End If
					EPDataHeader = -1
					EPDataLoc = 0
				End If
				
				With DataTable(CurrEPTable)
					'Get data
					EPTempData = Str(.Items)
					For CurrEPItem = 1 To .Items
						EPTempData = EPTempData + ", " + Str(.Item(CurrEPItem))
					Next
					
					'Add table
					ToAsmSymbols += 1
					ToAsmSymbol(ToAsmSymbols, 1) = "Table" + Trim(.Name)
					ToAsmSymbol(ToAsmSymbols, 2) = Str(EPDataLoc)
					
					CurrLine = LinkedListInsert(CurrLine, "Table" + Trim(.Name) + " equ " + Str(EPDataLoc))
					CurrLine = LinkedListInsert(CurrLine, " de " + EPTempData)
					EPDataLoc += (.Items + 1)
				End With
			End If
		Next
	End If
	
	CompilerOutput->CodeEnd = CurrLine
	
End Sub

Function Message (InData As String) As String
	
	Dim As String Temp, MsgOut, MsgID
	Dim As Integer DS, T
	
	'Find message
	MsgID = UCase(InData)
	DS = 0
	FOR T = 1 TO MSGC
		IF Messages(1, T) = MsgID THEN DS = T: EXIT FOR
	NEXT
	
	'Retrieve message
	IF DS = 0 THEN
		Return "Message " + MsgID + " not defined in messages.dat!"
	Else
		MsgOut = Messages(2, DS)
	End If
	
	'Replace values
	IF INSTR(UCase(MsgOut), "%VERSION%") <> 0 THEN Replace MsgOut, "%VERSION%", Version
	IF INSTR(UCase(MsgOut), "%FILENAME%") <> 0 THEN Replace MsgOut, "%FILENAME%", FI
	IF INSTR(UCase(MsgOut), "%OUTPUTFILE%") <> 0 THEN Replace MsgOut, "%OUTPUTFILE%", OFI
	IF INSTR(UCase(MsgOut), "%CHIPNAME%") <> 0 THEN Replace MsgOut, "%CHIPNAME%", ChipName
	
	Return MsgOut
END FUNCTION

Function ModeAVR As Integer
	If ChipFamily >= 100 And ChipFamily < 200 Then Return -1
	Return 0
End Function

Function ModePIC As Integer
	If ChipFamily > 0 And ChipFamily < 100 Then Return -1
	Return 0
End Function

Function ModeZ8 As Integer
	If ChipFamily > 200 And ChipFamily < 300 Then Return -1
	Return 0
End Function

Function NewCodeSection As CodeSection Pointer
	Dim As CodeSection Pointer OutSection
	
	OutSection = Callocate(SizeOf(CodeSection))
	OutSection->CodeList = LinkedListCreate
	OutSection->CodeEnd = OutSection->CodeList
	
	Return OutSection
End Function

Function NewProgLineMeta As ProgLineMeta Pointer
	Dim As ProgLineMeta Pointer OutMeta
	
	OutMeta = Callocate(SizeOf(ProgLineMeta))
	
	With (*OutMeta)
		.RequiredBank = -1
		
		.IsAutoPageSel = 0
		
		.PrevCommands = LinkedListCreate
		.NextCommands = LinkedListCreate
	End With
	
	Return OutMeta
End Function

Function NewSubroutine(SubName As String) As SubType Pointer
	Dim As SubType Pointer OutSub
	
	OutSub = Callocate(SizeOf(SubType))
	With (*OutSub)
		.Name = SubName
		.CodeStart = LinkedListCreate
		.CallList = LinkedListCreate
	End With
	
	Return OutSub
End Function

FUNCTION TypeOfVar (VarName As String, CurrSub As SubType Pointer) As String
	
	Dim As String Temp, TempType, Source
	Dim As String GlobalType, LocalType
	Dim As Integer PD, SubLoc
	Dim As SubType Pointer MainSub
	Dim As Single TempValue
	
	'Get pointer to main sub
	MainSub = Subroutine(0)
	'Do this to prevent null pointer access
	If CurrSub = 0 Then
		Print "Internal error in TypeOfVar"
		CurrSub = MainSub
	End If
	
	'Bit variables with . override all
	If INSTR(VarName, ".") <> 0 Then TypeOfVar = "BIT": Exit Function
	
	'Type overrides
	If INSTR(VarName, "[") <> 0 Then
		If INSTR(LCase(VarName), "[single]") <> 0 Then Return "SINGLE"
		If INSTR(LCase(VarName), "[integer]") <> 0 Then Return "INTEGER"
		If INSTR(LCase(VarName), "[word]") <> 0 Then Return "WORD"
		If INSTR(LCase(VarName), "[byte]") <> 0 Then Return "BYTE"
	End If
	
	'Handle constants
	If IsConst(VarName) Then
		If InStr(VarName, "@") <> 0 Then
			Return "WORD"
		Else
			TempValue = MakeDec(VarName)
			If TempValue = 0 Or TempValue = 1 Then Return "BIT"
			If TempValue >= 2 And TempValue <= 255 Then Return "BYTE"
			If TempValue >= 256 And TempValue <= 65535 Then Return "WORD"
			If TempValue >= -32768 And TempValue <= -1 Then Return "INTEGER"
			IF TempValue <> INT(TempValue) Then Return "SINGLE"
		End If
	End If
	
	'Temporary Calculation vars
	IF Left(UCase(VarName), 7) = "SYSTEMP" Then
		Temp = CalcVars(Val(Mid(VarName, 8))).CurrentType
		If Temp = "" Then Temp = "BYTE"
		Return Temp
	End If
	
	'Element is a function
	SubLoc = LocationOfSub(VarName, "")
	If SubLoc > 0 Then
		If Subroutine(SubLoc)->IsFunction Then	
			With *(Subroutine(SubLoc))	
				If .ReturnType <> "" Then
						Return .ReturnType
				'Else
				'		Return "BYTE"
				End If
			End With	
		End If
	End If
	
	'Get local type
	LocalType = "BYTE"
	GlobalType = "BYTE"
	If CurrSub <> MainSub Then
		
		'To proceed any further, sub needs to have dim commands compiled
		If Not CurrSub->VarsRead Then CompileDim(CurrSub)
		
		'Search var list
		FOR PD = 1 TO CurrSub->Variables
			With CurrSub->Variable(PD)
				IF UCase(.Name) = UCase(VarName) Then
					LocalType = Trim(UCase(.Type))
					Exit For
				End If
			End With
		Next
	End If
	
	'Permanent calculation vars
	If UCase(Left(VarName, 11)) = "SYSCALCTEMP" And LEN(VarName) = 12 And LocalType = "BYTE" Then
		Temp = UCase(Mid(VarName, 12, 1))
		TempType = ""
		IF Temp = "A" Then TempType = CalcTempType(1)
		IF Temp = "B" Then TempType = CalcTempType(2)
		IF Temp = "X" Then TempType = CalcTempType(3)
		IF TempType <> "" THEN Return TempType
	End If
	
	'Get global type
	For PD = 1 To MainSub->Variables
		With MainSub->Variable(PD)
			IF UCase(.Name) = UCase(VarName) Then
				GlobalType = Trim(UCase(.Type))
				Exit For
			End If
		End With
	Next
	
	'Get highest of local and global type
	If CastOrder(LocalType) > CastOrder(GlobalType) Then
		Return LocalType
	Else
		Return GlobalType
	EndIf
	
End FUNCTION

FUNCTION TypeOfValue (ValueNameIn As String, CurrentSub As SubType Pointer, SingCharString As Integer = 0) As String
	'SingCharString: 0 = treat single char as byte, -1 = treat single char as string
	
	Dim As String ThisType, FinalType, Temp, ValueTemp, CurrentItem, CurrChar
	Dim As String ValueName
	Dim As Integer TCC, SS, CD, FindSub, SubLoc
	Dim TempValue As Double
	Dim TypeCheck(20) As String
	TCC = 0
	FinalType = ""
	
	'If no value, don't return a type
	If ValueNameIn = "" Then Return ""
	ValueName = ValueNameIn
	
	'Type overrides
	If Left(ValueName, 1) = "[" And InStr(ValueName, "]") <> 0 Then
		ThisType = Mid(ValueName, 2)
		ThisType = Left(ThisType, InStr(ThisType, "]") - 1)
		Return UCase(Trim(ThisType))
	End If
	
	'Is whole thing a constant?
	If IsConst(ValueName) Then
		If InStr(ValueName, "@") <> 0 Then
			ThisType = "WORD"
		Else
			TempValue = MakeDec(ValueName)
			If TempValue = 0 Or TempValue = 1 Then
				ThisType = "BIT"
			ElseIf TempValue >= 2 And TempValue <= 255 Then
				 ThisType = "BYTE"
			ElseIf TempValue >= 256 And TempValue <= 65535 Then
				ThisType = "WORD"
			ElseIf TempValue >= -32768 And TempValue <= -1 Then
				ThisType = "INTEGER"
			ElseIf TempValue >= 0 And TempValue <= (2 ^ 32 - 1) Then
				ThisType = "LONG"
			End If
			
			IF TempValue <> INT(TempValue) Then ThisType = "SINGLE"
			'If InStr(UCase(ValueName), ";STRING") <> 0 Then ThisType = "STRING"
			If InStr(UCase(ValueName), ";STRING") <> 0 Then
				If Len(GetString(ValueName)) = 1 And Not SingCharString Then
					ThisType = "BYTE"
				Else
					ThisType = "STRING"
			End If
			End If
		End If
		Return ThisType
	End If
	
	'Remove function parameters and array indexes
	'Find (
	'Then check for an operator or start of string before (
	'If no operator, remove from ( to matching ) inclusive
	If InStr(ValueName, "(") Then
		Do While InStr(ValueName, " ") <> 0: Replace ValueName, " ", "": Loop
		Dim As Integer DelStart, DelEnd, DelLevel, DelPos
		DelPos = 1
		'Find (
		Do While DelPos < Len(ValueName)
			CurrChar = Mid(ValueName, DelPos, 1)
			If CurrChar = "(" Then
				'Is there a character before ( ?
				If DelPos <> 1 Then
					'Is there something other than a symbol before (
					If Not IsDivider(Mid(ValueName, DelPos - 1, 1)) Then
						'There is a something other than an operator before (
						'So we have found an array index or function param
						'Find end of index/params
						DelStart = DelPos
						DelLevel = 0
						DelEnd = DelStart
						Do While DelEnd <= Len(ValueName)
							CurrChar = Mid(ValueName, DelEnd, 1)
							If CurrChar = "(" Then
								DelLevel += 1
							ElseIf CurrChar = ")" Then
								DelLevel -= 1
								If DelLevel = 0 Then
									Exit Do
								End If
							End If
							DelEnd += 1
						Loop
						'Remove everything from start to end
						ValueName = Left(ValueName, DelStart - 1) + Chr(31) + Mid(ValueName, DelEnd + 1)
					End If
				End If
			End If
			
			DelPos += 1
		Loop 
	End If
	
	'Split into individual elements
	CurrentItem = ""
	For SS = 1 To LEN(ValueName)
		Temp = Mid(ValueName, SS, 1)
		If IsDivider(Temp) AND Temp <> "." AND Temp <> ";" AND Temp <> "$" And Temp <> "[" And Temp <> "]" Then
			IF Trim(CurrentItem) <> "" Then
				TCC += 1
				TypeCheck(TCC) = Trim(CurrentItem)
				CurrentItem = ""
			End If
		Else
			CurrentItem = CurrentItem + Temp
		End If
	Next
	If Trim(CurrentItem) <> "" Then
		TCC += 1
		TypeCheck(TCC) = Trim(CurrentItem)
	End If

	'Check each element
	Dim HasIndex As Integer
	For CD = 1 To TCC
		ValueTemp = TypeCheck(CD)
		HasIndex = 0
		If Right(ValueTemp, 1) = Chr(31) Then
			HasIndex = -1
			ValueTemp = Left(ValueTemp, Len(ValueTemp) - 1)
		End If
		ThisType = ""
		
		'Element is a string const
		If INSTR(ValueTemp, ";STRING") <> 0 Then
			If Len(GetString(ValueTemp)) = 1 And Not SingCharString Then
				ThisType = "BYTE"
			Else
				ThisType = "STRING"
			End If
		
		'Element has a cast
		ElseIf InStr(ValueTemp, "[") <> 0 And InStr(ValueTemp, "]") <> 0 Then
			ValueTemp = Mid(ValueTemp, InStr(ValueTemp, "[") + 1)
			ValueTemp = Left(ValueTemp, InStr(ValueTemp, "]") - 1)
			ThisType = UCase(Trim(ValueTemp))
			
		'Element is a const
		ElseIf IsConst(ValueTemp) Then
			If InStr(ValueTemp, "@") <> 0 Then
				ThisType = "WORD"
			Else
				TempValue = MakeDec(ValueTemp)
				If TempValue = 0 Or TempValue = 1 Then
					ThisType = "BIT"
				ElseIf TempValue >= 2 And TempValue <= 255 Then
					 ThisType = "BYTE"
				ElseIf TempValue >= 256 And TempValue <= 65535 Then
					ThisType = "WORD"
				ElseIf TempValue >= -32768 And TempValue <= -1 Then
					ThisType = "INTEGER"
				ElseIf TempValue >= 0 And TempValue <= (2 ^ 32 - 1) Then
					ThisType = "LONG"
				End If
				
				IF TempValue <> INT(TempValue) Then ThisType = "SINGLE"
			End If
			
		'Element is a string var, ending with $
		ElseIf Right(ValueTemp, 1) = "$" Then
			ThisType = "STRING"
			If HasIndex Then ThisType = "BYTE"
		
		'Element is a bit var
		ElseIf InStr(ValueTemp, ".") <> 0 And Not IsConst(Left(ValueTemp, InStr(ValueTemp, ".") - 1)) Then
			ThisType = "BIT"
			
		'Element is a var (or function result)
		Else
			ThisType = TypeOfVar(ValueTemp, CurrentSub)
			'May be accessing character of string
			If HasIndex And ThisType = "STRING" Then
				'Check for function
				SubLoc = LocationOfSub(ValueTemp, "", "", -1)
				
				If SubLoc = 0 Then
					'No function, so accessing char of string - type is byte
					ThisType = "BYTE"
				Else
					ThisType = Subroutine(SubLoc)->ReturnType
				End If
			End If
		End If
		
		'Set FinalType to highest value
		'Print ,, ValueTemp + " is " + ThisType
		If CastOrder(ThisType) > CastOrder(FinalType) Then FinalType = ThisType
		
	Next
	
	Return FinalType

END FUNCTION

Sub UpdateOutgoingCalls (CompSub As SubType Pointer)
	'Update call counts to other subroutines
	'Use after allocating a subroutine to a program memory page
	Dim As SubType Pointer CalledSub
	Dim As LinkedListElement Pointer ListItem
	
	'Make sure that this subroutine is on a valid page
	If CompSub->DestPage < 0 Or CompSub->DestPage > MAX_PROG_PAGES Then
		Exit Sub
	End If
	
	'Find all subroutines called from this one
	ListItem = CompSub->CallList->Next
	Do While ListItem <> 0
		CalledSub = ListItem->Metadata
		'Update call counts
		CalledSub->CallsFromPage(CompSub->DestPage) += ListItem->NumVal
		ListItem = ListItem->Next
	Loop
End Sub

Sub UpgradeCalcVar (VarName As String, VarType As String)
	'Upgrade the type of a calc var
	Dim As String Temp
	Dim As Integer CV, CD
	
	If VarName <> "" Then
		'If name specified, only mark that var
		
		'Exit if input is not a calc var
		IF Left(UCase(VarName), 7) <> "SYSTEMP" Then Exit Sub
		
		Temp = Mid(Trim(VarName), 8)
		CV = VAL(Temp)
		
		With CalcVars(CV)
			If CastOrder(.CurrentType) < CastOrder(VarType) Then
				.CurrentType = VarType
				'Also need to change max type
				If CastOrder(VarType) > CastOrder(.MaxType) Then
					.MaxType = VarType
				End If
			End If
		End With
	End If
End Sub

FUNCTION VarAddress (ArrayNameIn As String, CurrSub As SubType Pointer) As VariableType Pointer
	Dim As String ArrayName
	Dim As Integer FA, LO
	Dim As SubType Pointer MainSub
	MainSub = Subroutine(0)
	
	'Tidy name
	ArrayName = ArrayNameIn
	If INSTR(ArrayNameIn, "(") <> 0 Then ArrayName = RTrim(Left(ArrayNameIn, INSTR(ArrayNameIn, "(") - 1))
	If INSTR(ArrayNameIn, "$") <> 0 Then ArrayName = RTrim(Left(ArrayNameIn, INSTR(ArrayNameIn, "$") - 1))
	
	'Search local variables
	FOR FA = 1 TO CurrSub->Variables
		IF ArrayName = UCase(CurrSub->Variable(FA).Name) THEN Return @(CurrSub->Variable(FA))
	NEXT
	
	'Search global variables
	If MainSub <> CurrSub Then
		FOR FA = 1 TO MainSub->Variables
			IF ArrayName = UCase(MainSub->Variable(FA).Name) THEN Return @(MainSub->Variable(FA))
		Next
	End If
	
	'Check if variable is a function result
	LO = LocationOfSub(ArrayNameIn, "", "")
	'Print ArrayNameIn, LO
	If LO <> 0 Then
		With *Subroutine(LO)
			If .IsFunction Then
				'Have found function, add a var and then return it
				AddVar ArrayNameIn, .ReturnType, 1, 0, "REAL", ""
				FOR FA = 1 TO MainSub->Variables
					IF ArrayName = UCase(MainSub->Variable(FA).Name) THEN Return @(MainSub->Variable(FA))
				Next
			End If
		End With
	End If
	
	'Nothing found, return null pointer
	'Print "Var " + ArrayName + " not found in sub " + CurrSub->Name
	Return 0
END FUNCTION



