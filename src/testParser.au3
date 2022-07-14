;#NoTrayIcon
#AutoIt3Wrapper_Run_AU3Check=n

;Global $x = []

;MsgBox(0, "", VarGetType($x))

#include <parser.au3>


;$tree = parser_parse(FileRead(@ScriptFullPath))

;#cs
$sFile = "C:\Users\Frank\Documents\GitHub\AutoItObject Internal\AutoItObject_Internal.au3"
;$sFile = @ScriptDir&"/parser.au3"
;$sFile = @ScriptFullPath
;$sFile = @ScriptDir&"/../test.au3"
$aLexer = lexer(FileRead($sFile))
$hTimer = TimerInit()
While 1
    $aToken = lexer_nextToken($aLexer)
    If @error <> 0 Then ExitLoop
    ;ConsoleWrite('"'&StringReplace(StringFromASCIIArray(($aLexer[$LEXER_READER])[$READER_DATA], ($aToken[$LEXERTOKEN_START])[0], ($aToken[$LEXERTOKEN_END])[0]), @LF, "@NEWLINE@")&'"'&@CRLF)
WEnd
If @error <> 0 And ($aToken <> -1 Or IsString($aToken)) Then
    ConsoleWrite($sFile&" "&$aLexer[$LEXER_LINE]&":"&$aLexer[$LEXER_COL]&@CRLF)
    ConsoleWrite(@extended&@CRLF&$aToken&@CRLF)
Else
    ConsoleWrite(TimerDiff($hTimer)&@CRLF)
EndIf

;ConsoleWrite($iStep&@CRLF)
;#ce