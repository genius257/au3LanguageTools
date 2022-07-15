#AutoIt3Wrapper_Run_AU3Check=n
;#include <Array.au3>

Global Enum $READER_DATA, $READER_LENGTH, $READER_INDEX, $READER_MAX

Func reader($sInput)
    Local $aReader[$READER_MAX]
    $aReader[$READER_DATA] = StringToASCIIArray($sInput)
    $aReader[$READER_INDEX] = 0
    $aReader[$READER_LENGTH] = StringLen($sInput)
    return $aReader
EndFunc

Func reader_peek(ByRef $aReader, $iIndex = 0)
    If ($aReader[$READER_INDEX] + $iIndex) >= $aReader[$READER_LENGTH] Then Return SetError(1, @ScriptLineNumber, -1)
    Return ($aReader[$READER_DATA])[$aReader[$READER_INDEX] + $iIndex]
EndFunc

Func reader_consume(ByRef $aReader)
    Local $iChar = reader_peek($aReader)
    $aReader[$READER_INDEX] += 1
    Return SetError(@error, @extended, $iChar)
EndFunc

Func reader_skip(ByRef $aReader, $iChars)
    If $aReader[$READER_INDEX] >= $aReader[$READER_LENGTH] Then Return SetError(1, @ScriptLineNumber, -1)
    $aReader[$READER_INDEX] += $iChars
EndFunc

Global Enum $LEXER_READER, $LEXER_POS, $LEXER_LINE, $LEXER_COL, $LEXER_MAX

Func lexer($sInput)
    Local $aLexer[$LEXER_MAX]
    $aLexer[$LEXER_READER] = reader($sInput)
    ;$aLexer[$LEXER_POS] = 0
    $aLexer[$LEXER_LINE] = 1
    $aLexer[$LEXER_COL] = 0
    Return $aLexer
EndFunc

Global Const $LEXER_CHAR_CR = AscW(@CR), $LEXER_CHAR_LF = AscW(@LF), $LEXER_CHAR_LT = AscW("<")
Global Enum $LEXER_TOKEN_UNKNWON, $LEXER_TOKEN_IDENTIFIER, $LEXER_TOKEN_VARIABLE, $LEXER_TOKEN_PREPROC, $LEXER_TOKEN_WHITESPACE, $LEXER_TOKEN_NEWLINE, $LEXER_TOKEN_OPERATOR, $LEXER_TOKEN_DOT, $LEXER_TOKEN_STRING, $LEXER_TOKEN_MACRO, $LEXER_TOKEN_PARENTHESIS, $LEXER_TOKEN_NUMBER, $LEXER_TOKEN_SINGLE_LINE_COMMENT, $LEXER_TOKEN_MULTI_LINE_COMMENT, $LEXER_TOKEN_SQUARE_BRACKET, $LEXER_TOKEN_COMMA, $LEXER_TOKEN_INCLUDE, $LEXER_TOKEN_INCLUDE_PATH, $LEXER_TOKEN_INCLUDE_ONCE, $LEXER_TOKEN_HEX

Func lexer_nextToken(ByRef $aLexer)
    Local $aPos = lexer_pos($aLexer)
    Local $iChar = lexer_nextChar($aLexer)
    Local $iToken = $LEXER_TOKEN_UNKNWON
    If @error <> 0 Then Return SetError(@error, @extended, $iChar)
    Switch $iChar
        Case 65 To 90, 95, 97 To 122
            Local $iPeek = 0
            While 1
                $iChar = reader_peek($aLexer[$LEXER_READER], $iPeek)
                If Not (StringIsAlNum(ChrW($iChar)) Or $iChar = 95) Then ExitLoop
                ;If Not (($iChar >= 65 And $iChar <= 90) Or ($iChar >= 97 And $iChar <= 122) Or $iChar = 95) Then ExitLoop
                $iPeek +=1
            WEnd
            lexer_skip($aLexer, $iPeek)
            $iToken = $LEXER_TOKEN_IDENTIFIER
        Case 9, 32
            Local $iPeek = 0
            While 1
                $iChar = reader_peek($aLexer[$LEXER_READER], $iPeek)
                If Not ($iChar = 32 Or $iChar = 9) Then ExitLoop
                $iPeek+=1
            WEnd
            lexer_skip($aLexer, $iPeek)
            $iToken = $LEXER_TOKEN_WHITESPACE
        Case 35;#
            If lexer_isPeekEqualStringCaseInsensitive($aLexer, "cs") Or lexer_isPeekEqualStringCaseInsensitive($aLexer, "comments-start") Then
                Local $aToken
                While 1
                    lexer_skipToNewline($aLexer)
                    If @error <> 0 Then Return SetError(1, @ScriptLineNumber, "Unterminated multi-line comment")
                    lexer_skip($aLexer, lexer_peekToken($aLexer) = $LEXER_CHAR_CR ? 2 : 1)
                    ;If @error <> 0 Then Return SetError(@error, @extended, $iChar)
                    $iChar = reader_peek($aLexer[$LEXER_READER])
                    If $iChar = $LEXER_CHAR_LF Or $iChar = $LEXER_CHAR_CR And reader_peek($aLexer[$LEXER_READER], 1) = $LEXER_CHAR_LF Then ContinueLoop
                    $aToken = lexer_nextToken($aLexer)
                    If @error = 0 And $aToken[0] = $LEXER_TOKEN_WHITESPACE Then $aToken = lexer_nextToken($aLexer)
                    ;If @error = 0 Then ConsoleWrite("iToken: "&$aToken[0]&@CRLF)
                    ;If reader_peek($aLexer[$LEXER_READER]) = 35 Then; And () Then
                    If @error = 0 And $aToken[0] = $LEXER_TOKEN_PREPROC And (lexer_isPeekEqualStringCaseInsensitive($aLexer, "ce", ($aToken[1])[0]-($aToken[2])[0]+1) Or lexer_isPeekEqualStringCaseInsensitive($aLexer, "comments-end", ($aToken[1])[0]-($aToken[2])[0]+1)) Then
                        ;ConsoleWrite(StringFromASCIIArray(($aLexer[$LEXER_READER])[$READER_DATA], ($aToken[1])[0]+1, ($aToken[1])[0]+3)&@CRLF)
                        ;Exit
                        $iToken = $LEXER_TOKEN_MULTI_LINE_COMMENT
                        ExitLoop
                    EndIf
                WEnd
            Else
                $iChar = lexer_skipToNewline($aLexer)
                If @error <> 0 Then Return SetError(@error, @extended, $iChar)
                $iToken = $LEXER_TOKEN_PREPROC
            EndIf
            ;ConsoleWrite(StringFromASCIIArray(($aLexer[$LEXER_READER])[$READER_DATA], $aPos[0]+1, $aPos[0]+15)&@CRLF)
        Case $LEXER_CHAR_CR
            $iToken = $LEXER_TOKEN_UNKNWON
            If reader_peek($aLexer[$LEXER_READER]) = $LEXER_CHAR_LF Then ContinueCase;FIXME: should skip 1 before continue case.
        Case $LEXER_CHAR_LF
            $iToken = $LEXER_TOKEN_NEWLINE
        Case 60 To 62, 38, 43, 45, 63, 58, 42, 47
            $iToken = $LEXER_TOKEN_OPERATOR
        Case 46
            $iToken = $LEXER_TOKEN_DOT
        Case 34, 39;FIXME: add support for $iChar within string if occuring twice in a row.
            Local $iPeek = 0
            Local $iChar2 = -1
            While 1
                $iChar2 = reader_peek($aLexer[$LEXER_READER], $iPeek)
                If ($iChar2 == $iChar) Or ($iChar2 == $LEXER_CHAR_LF) Then ExitLoop
                $iPeek+=1
            WEnd
            If Not ($iChar2 = $iChar) Then Return SetError(1, @ScriptLineNumber, "Unterminated string")
            lexer_skip($aLexer, $iPeek+1)
            $iToken = $LEXER_TOKEN_STRING
        Case 36; $
            $iChar = reader_peek($aLexer[$LEXER_READER])
            Local $iPeek = 0
            If StringIsAlpha(ChrW($iChar)) Or $iChar = 95 then
                $iPeek+=1
                ;lexer_skip($aLexer, 1)
                ;While StringIsAlNum(ChrW(reader_peek($aLexer[$LEXER_READER]))) Or reader_peek($aLexer[$LEXER_READER]) = 95
                While 1
                    $iChar = reader_peek($aLexer[$LEXER_READER], $iPeek)
                    If Not (StringIsAlNum(ChrW($iChar)) Or $iChar = 95) Then ExitLoop
                    ;lexer_skip($aLexer, 1)
                    $iPeek+=1
                WEnd
                lexer_skip($aLexer, $iPeek)
            Else
                Return SetError(1, @ScriptLineNumber, "Expected variable name")
            EndIf
            $iToken = $LEXER_TOKEN_VARIABLE
        Case 64; @
            ;Local $iPeek = 0
            While StringIsAlNum(ChrW(reader_peek($aLexer[$LEXER_READER]))) Or reader_peek($aLexer[$LEXER_READER]) = 95
                lexer_skip($aLexer, 1)
            WEnd
            ;lexer_skip($aLexer, $iPeek+1)
            $iToken = $LEXER_TOKEN_MACRO
        Case 40 To 41; ()
            $iToken = $LEXER_TOKEN_PARENTHESIS
        Case 48; 0
            $iChar = reader_peek($aLexer[$LEXER_READER])
            If Not ($iChar = 120 Or $iChar = 88) Then ContinueCase
            Local $iPeek = 0
            While 1
                $iChar = reader_peek($aLexer[$LEXER_READER], $iPeek+1)
                ;If Not (($iChar >= 48 And $iChar <= 57) Or ($iChar >= 97 And $iChar <= 102) Or ($iChar >= 65 And $iChar <= 70)) Then ExitLoop
                If Not StringIsXDigit(ChrW(reader_peek($aLexer[$LEXER_READER], $iPeek+1))) Then ExitLoop
                $iPeek += 1
            WEnd
            lexer_skip($aLexer, $iPeek+1)
            ;ConsoleWrite(StringFromASCIIArray(($aLexer[$LEXER_READER])[$READER_DATA], $aPos[0], lexer_pos($aLexer)[0])&@CRLF)
            $iToken = $LEXER_TOKEN_HEX
        Case 48 To 57; 0-9
            $iPeek = 0
            While 1
                If Not StringIsDigit(ChrW(reader_peek($aLexer[$LEXER_READER], $iPeek))) Then ExitLoop
                $iPeek+=1
            WEnd
            lexer_skip($aLexer, $iPeek)
            $iToken = $LEXER_TOKEN_NUMBER
        Case 59; semi-colon
            lexer_skipToNewline($aLexer)
            $iToken = $LEXER_TOKEN_SINGLE_LINE_COMMENT
        Case 91, 93; []
            $iToken = $LEXER_TOKEN_SQUARE_BRACKET
        Case 44; ,
            $iToken = $LEXER_TOKEN_COMMA
        Case Else
            Return SetError(1, @ScriptLineNumber, "UNKNOWN CHAR: "&ChrW($iChar)&" ("&$iChar&")")
    EndSwitch
    Return lexerToken($iToken, $aPos, lexer_pos($aLexer))
EndFunc

Func lexer_peekToken(ByRef $aLexer)
    Local $lpos = $aLexer[$LEXER_POS], $lline = $aLexer[$LEXER_LINE], $lcol = $aLexer[$LEXER_COL]
    Local $rpos = ($aLexer[$LEXER_READER])[$READER_INDEX]
    Local $aToken = lexer_nextToken($aLexer)
    $aLexer[$LEXER_POS] = $lpos
    $aLexer[$LEXER_LINE] = $lline
    $aLexer[$LEXER_COL] = $lcol
    ($aLexer[$LEXER_READER])[$READER_INDEX] = $rpos
EndFunc

Func lexer_nextChar(ByRef $aLexer)
    Local $iChar = reader_consume($aLexer[$LEXER_READER])
    If @error <> 0 Then Return SetError(@error, @extended, $iChar)
    $aLexer[$LEXER_COL] += 1
    If $iChar = $LEXER_CHAR_LF Then
        $aLexer[$LEXER_LINE] += 1
        $aLexer[$LEXER_COL] = 0
    EndIf
    return $iChar
EndFunc

Func lexer_skip(ByRef $aLexer, $iChars)
    $aLexer[$LEXER_COL] += $iChars
    reader_skip($aLexer[$LEXER_READER], $iChars)
EndFunc

Func lexer_skipToNewline(ByRef $aLexer)
    Local $aToken
    Local $iChar
    While 1
        $iChar = reader_peek($aLexer[$LEXER_READER])
        If $iChar = $LEXER_CHAR_CR And reader_peek($aLexer[$LEXER_READER], 1) = $LEXER_CHAR_LF Then Return Null
        If @error <> 0 Then Return SetError(1, @ScriptLineNumber, $iChar)
        If $iChar = $LEXER_CHAR_LF Then Return Null
        reader_skip($aLexer[$LEXER_READER], 1)
    WEnd
EndFunc

Func lexer_skipToChar(ByRef $aLexer, $iChar)
    While 1
        If reader_peek($aLexer[$LEXER_READER]) = $iChar Then ExitLoop
        If @error <> 0 Then Return SetError(1, @ScriptLineNumber, -1)
        reader_skip($aLexer[$LEXER_READER], 1)
    WEnd
EndFunc

Func lexer_pos(ByRef $aLexer)
    Local $aPos = [($aLexer[$LEXER_READER])[$READER_INDEX], $aLexer[$LEXER_LINE], $aLexer[$LEXER_COL]]
    Return $aPos
EndFunc

Func lexer_isPeekEqualString(ByRef $aLexer, $sString, $iOffset = 0)
    Local $aString = StringToASCIIArray($sString)
    ;ConsoleWrite($sString&' <=> "')
    For $i = 0 To UBound($aString, 1)-1
        ;ConsoleWrite(ChrW(reader_peek($aLexer[$LEXER_READER], $i+$iOffset)))
        ;If Not ($aString[$i] = reader_peek($aLexer[$LEXER_READER], $i+$iOffset)) Then ConsoleWrite('"'&@CRLF)
        If Not ($aString[$i] = reader_peek($aLexer[$LEXER_READER], $i+$iOffset)) Then Return False
    Next
    ;ConsoleWrite('"'&@CRLF)
    Return True
EndFunc

Func lexer_isPeekEqualStringCaseInsensitive(ByRef $aLexer, $sString, $iOffset = 0)
    Local $aString1 = StringToASCIIArray(StringLower($sString))
    Local $aString2 = StringToASCIIArray(StringUpper($sString))
    Local $iChar
    For $i = 0 To UBound($aString1, 1)-1
        $iChar = reader_peek($aLexer[$LEXER_READER], $i+$iOffset)
        If Not ($aString1[$i] = $iChar Or $aString2[$i] = $iChar) Then Return False
    Next
    Return True
EndFunc

Global Enum $LEXERTOKEN_TYPE, $LEXERTOKEN_START, $LEXERTOKEN_END, $LEXERTOKEN_MAX

Func lexerToken($iType, ByRef $start, ByRef $end)
    Local $aLexertoken = [$iType, $start, $end]
    Return $aLexertoken
EndFunc

Func lexerToken_toString($sInput, ByRef $aToken)
    Return StringMid($sInput, ($aToken[1])[0] + 1, ($aToken[2])[0]-($aToken[1])[0])
EndFunc

Func parser_parse($sInput)
    $aLexer = lexer($sInput)
    While 1
        $aToken = lexer_nextToken($aLexer)
        If @error <> 0 Then ExitLoop
        Switch $aToken[0]
            Case $LEXER_TOKEN_IDENTIFIER
                ;ConsoleWrite(lexerToken_toString($sInput, $aToken)&@CRLF)
                Switch StringLower(StringMid($sInput, ($aToken[1])[0], ($aToken[2])[0]))
                    Case 'func'
                    Case 'local', 'global'
                    Case Else
                EndSwitch
            Case $LEXER_TOKEN_NEWLINE, $LEXER_TOKEN_SINGLE_LINE_COMMENT, $LEXER_TOKEN_WHITESPACE
                ContinueLoop
            Case $LEXER_TOKEN_VARIABLE
                $aToken = lexer_nextToken($aLexer)
                    If $aToken[0] = $LEXER_TOKEN_WHITESPACE Then $aToken = lexer_nextToken($aLexer)
            Case $LEXER_TOKEN_PREPROC
                Switch StringLower(lexerToken_toString($sInput, $aToken))
                    Case "#include"
                        $aToken = lexer_nextToken($aLexer)
                        If $aToken[0] = $LEXER_TOKEN_WHITESPACE Then $aToken = lexer_nextToken($aLexer)
                        
                        If Not (($aToken[0] = $LEXER_TOKEN_OPERATOR) And lexerToken_toString($sInput, $aToken) = "<") Then
                            ConsoleWriteError("Expected whitespace or lt operator"&@CRLF)
                            Exit
                        EndIf

                        $aToken = parser_skipToToken($aLexer, $LEXER_TOKEN_OPERATOR)
                        If @error <> 0 Or Not lexerToken_toString($sInput, $aToken) = ">" Then
                            ConsoleWrite(lexerToken_toString($sInput, $aToken)&@CRLF)
                            Exit ConsoleWriteError("Expected gt operator"&@CRLF)
                        EndIf
                    Case Else
                EndSwitch
            Case Else
                ConsoleWrite(($aToken[1])[1]&":"&($aToken[1])[2]&@CRLF)
                ConsoleWrite(lexerToken_toString($sInput, $aToken)&@CRLF)
                ConsoleWriteError("UNSUPPORTED TOKEN: "&$aToken[0]&@CRLF)
                Exit
        EndSwitch
    WEnd
EndFunc

Func parser_isKeyword($sString)
    Switch StringLower($sString)
        Case "and", "byref", "case", "const", "continuecase", "continueloop", "default", "dim", "do", "else", "elseif", "endfunc", "endif", "endselect", "endswitch", "endwith", "enum", "exit", "exitloop", "for", "func", "global", "if", "in", "local", "next", "not", "or", "redim", "return", "select", "static", "step", "switch", "then", "to", "until", "volatile", "wend", "while", "with"
            Return True
        Case Else
            Return False
    EndSwitch
EndFunc

Func parser_isReservedIdentifier($sIdentifier)
    If parser_isKeyword($sIdentifier) Then Return True
    Switch StringLower($sIdentifier)
        Case "null", "true", "false"
            Return True
        Case Else
            Return False
    EndSwitch
EndFunc

Func parser_expectToken(ByRef $aLexer, $iToken)
    Local $aToken = lexer_peekToken($aLexer)
    If @error <> 0 Then Return SetError(1, @ScriptLineNumber, False)
    Return $aToken[0] = $iToken
EndFunc

Func parser_skipToToken(ByRef $aLexer, $iToken)
    Local $aToken
    While 1
        $aToken = lexer_nextToken($aLexer)
        If @error <> 0 Then Return SetError(1, @ScriptLineNumber, $aToken)
        If $aToken[0] = $iToken Then Return $aToken
    WEnd
EndFunc

Func parser_skipToEOS(ByRef $aLexer)
    While 1
    WEnd
EndFunc
