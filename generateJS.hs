{-
Javascript Generator


Generates varied syntactically valid javascript according to the ECMA-262
specification: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-262.pdf

The code is structured to be very clearly analogous to the productions therein,
with as few deviations as possible.  The major deviations you will notice arise
when unicode is called for.  In the future it would be worthwhile to determine
what unicode productions are safe in modern js engines.



requires Control.Monad.Omega.  
`cabal install control-monad-omega` should fetch and install it.

ghc --make generateJS.hs -o generatejs -O2 && ./generatejs

-}

{-# OPTIONS_GHC -fglasgow-exts #-}
module Main
  where
import Control.Monad.Omega
import qualified Data.Set as Set
import Prelude hiding (catch)
import System.Directory
import System
import Text.Printf

data Symbol
    = Terminal String
    | Nonterminal [[Symbol]] -- a disjunction of juxtapositions
    | NotImplemented --todo: remove when everything is implemented

class SymbolLike a where
    toSymbol :: a -> Symbol
instance SymbolLike Symbol where
    toSymbol = id
instance SymbolLike String where
    toSymbol = Terminal
instance SymbolLike Char where
    toSymbol c = Terminal [c]

--Take a Symbol and enumerate every expansion of it
getAll s = map concat $ runOmega $ enumerate s

oneOf :: SymbolLike a => [a] -> Symbol
oneOf ts = Nonterminal $ map (\f -> [toSymbol f]) ts

many1 :: Symbol -> Symbol
many1 s = n
    where n = Nonterminal [[s], [n,s]]

many s = m
    where m = Nonterminal [[], [m,s]]

optional s = Nonterminal [[], [s]]

enumerate :: Symbol -> Omega [String]
enumerate (NotImplemented) = return []
enumerate (Terminal a) = return [a]
enumerate (Nonterminal alts) = do
    alt <- each alts          -- for each alternative
      -- (each is the Omega constructor :: [a] -> Omega a)
    rep <- mapM enumerate alt -- enumerate each symbol in the sequence
    return $ concat rep       -- and concatenate the results

--Returns the symbol given by removing every element
--from the set from the symbol
butNot :: SymbolLike a => Symbol -> a -> Symbol
butNot s n = oneOf (filter (\c -> not $ Set.member c set) (getAll s))
    where set = Set.fromList $ getAll $ toSymbol n


--Generation code:



--todo: figure out the subset of unicode that's widely supported and stick to that
sourceCharacter = oneOf $ ['a'..'z']


----Unicode Format-Control Characters
--todo

----White space
whiteSpace = oneOf [" ", "\t"] --todo: ++ <VT> <FF> <NBSP> <USP>

----Line terminators
lineTerminator = oneOf ["\r", "\n"] --todo: <LS> <PS>

----Comments
--todo

----Tokens
token = oneOf [reservedWord, identifier, punctuator, numericLiteral, stringLiteral]


reservedWord = oneOf [keyword, 
                      futureReservedWord,
                      nullLiteral,
                      booleanLiteral]

keyword = oneOf ["break","else","new","var","case","finally","return",
            "void","catch","for","switch","while","continue",
            "function","this","with","default","if","throw",
            "delete","in","try","do","instanceof","typeof"]

futureReservedWord = oneOf ["abstract","enum","int","short","boolean","export",
                   "interface","static","byte","extends","long","super",
                   "char","final","native","synchronized","class","float",
                   "package","throws","const","goto","private","transient",
                   "debugger","implements","protected","volatile","double",
                   "import","public"]

----Identifiers
identifier = identifierName `butNot` reservedWord

identifierName = Nonterminal [[identifierStart], 
                              [identifierName, identifierPart]]
identifierStart = oneOf [oneOf ["_", "$"], unicodeLetter]
identifierPart = oneOf [identifierStart, unicodeDigit, unicodeEscape]
                              -- ++ [unicodeCombinindMark, unicodeConnectorPunctuation]

--todo: actual unicode
unicodeLetter = oneOf $ ['a'..'z'] ++ ['A'..'Z'] 
unicodeEscape = Nonterminal [[Terminal "\\", unicodeEscapeSequence]]
unicodeDigit = decimalDigit
--UnicodeConnectorPunctuation = 
--UnicodeCombiningMark = 

hexDigit = oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']


----Punctuators
punctuator = oneOf ["{","}","(",")","[","]",".",";",",","<",">","<=",">=",
                    "==","!=","===","!==","+","-","*","%","++","--","<<",">>",">>>",
                    "&","|","^","!","~","&&","||","?",":","=","+=","-=","*=","%=",
                    "<<=",">>=",">>>=","&=","|=","^="]
divPunctuator = oneOf ["/", "/="]


---- Literals

literal = oneOf [nullLiteral, booleanLiteral, numericLiteral]
nullLiteral = Terminal "null"
booleanLiteral = oneOf ["true", "false"]
numericLiteral = oneOf [decimalLiteral, hexIntegerLiteral]
decimalLiteral = Nonterminal [[decimalIntegerLiteral, Terminal ".", optional decimalDigits, optional exponentPart],
                              [Terminal ".", decimalDigits, optional exponentPart],
                              [decimalIntegerLiteral, optional exponentPart]]

decimalIntegerLiteral = Nonterminal [[Terminal "0"],
                                     [nonZeroDigit, optional decimalDigits]]

decimalDigits = many1 decimalDigit
decimalDigit = oneOf $ map show [0..9]
nonZeroDigit = oneOf $ map show [1..9]

exponentPart = Nonterminal [[exponentIndicator,signedInteger]]
exponentIndicator = oneOf ["e","E"]
signedInteger = Nonterminal [[decimalDigits],
                             [Terminal "-", decimalDigits],
                             [Terminal "+", decimalDigits]]

hexIntegerLiteral = Nonterminal [[Terminal "0x", hexDigit],
                                 [Terminal "0X", hexDigit],
                                 [hexIntegerLiteral, hexDigit]]



stringLiteral = Nonterminal [[Terminal "\"", many doubleStringCharacter, Terminal "\""],
                             [Terminal "'" , many singleStringCharacter, Terminal "'" ]]

doubleStringCharacter = Nonterminal [[sourceCharacter `butNot` (oneOf [Terminal "\"", Terminal "\\", lineTerminator])],
                                     [Terminal "\\", escapeSequence]]
singleStringCharacter = Nonterminal [[sourceCharacter `butNot` (oneOf [Terminal "\'", Terminal "\\", lineTerminator])],
                                     [Terminal "\\", escapeSequence]]

escapeSequence = oneOf [characterEscapeSequence, hexEscapeSequence, unicodeEscapeSequence]
                --todo: 0 [next character is not decimalDigit]

characterEscapeSequence = oneOf [singleEscapeCharacter, nonEscapeCharacter]
singleEscapeCharacter = oneOf ["'", "\"", "\\", "b", "f", "n", "r", "t", "v"]
nonEscapeCharacter = sourceCharacter `butNot` (oneOf [escapeCharacter, lineTerminator])
escapeCharacter = oneOf [singleEscapeCharacter, decimalDigit, Terminal "x", Terminal "u"] --not specified, but this seems right
hexEscapeSequence = Nonterminal [[Terminal "x", hexDigit, hexDigit]]

unicodeEscapeSequence = Nonterminal [[Terminal "u", hexDigit, hexDigit, hexDigit, hexDigit]]

regularExpressionLiteral = Nonterminal [[Terminal "/", regularExpressionBody, Terminal "/", regularExpressionFlags]]
regularExpressionBody = Nonterminal [[regularExpressionFirstChar, regularExpressionChars]]
regularExpressionChars = many regularExpressionChar
regularExpressionFirstChar = Nonterminal [[nonTerminator `butNot` (oneOf ["*", "\\", "/"])],
                                          [backslashSequence]]
regularExpressionChar = Nonterminal [[nonTerminator `butNot` (oneOf ["\\", "/"])],
                                     [backslashSequence]]
backslashSequence = Nonterminal [[Terminal "\\", nonTerminator]]

nonTerminator = sourceCharacter `butNot` lineTerminator

regularExpressionFlags = many identifierPart


-----Expresions

----Primary expressions

primaryExpression = Nonterminal [[Terminal "this"],
                                 [identifier],
                                 [literal],
                                 [arrayLiteral],
                                 [objectLiteral],
                                 [Terminal "(", expression, Terminal ")"]]

arrayLiteral = Nonterminal [[Terminal "[", optional elision, Terminal "]"],
                            [Terminal "[", elementList, Terminal "]"],
                            [Terminal "[", elementList, optional elision, Terminal "]"]]

elementList = Nonterminal [[assignmentExpression],
                           [elision, assignmentExpression],
                           [elementList, Terminal ",", assignmentExpression],
                           [elementList, Terminal ",", elision, assignmentExpression]]

elision = many1 $ Terminal ","


objectLiteral = Nonterminal [[Terminal "{", Terminal "}"],
                             [Terminal "{", propertyNameAndValueList, Terminal "}"]]

propertyNameAndValueList = Nonterminal [[propertyName, Terminal ":", assignmentExpression],
                                        [propertyNameAndValueList, Terminal ",", propertyName, Terminal ":", assignmentExpression]]

propertyName = oneOf [identifier, stringLiteral, numericLiteral]

----Left-Hand Side Expressions
memberExpression = Nonterminal [[primaryExpression],
                                [functionExpression],
                                [memberExpression, Terminal "[", expression, Terminal "]"],
                                [memberExpression, Terminal ".", identifier],
                                [Terminal "new ", memberExpression, arguments]]

newExpression = Nonterminal [[memberExpression],
                             [Terminal "new ", newExpression]]

callExpression = Nonterminal [[memberExpression, arguments],
                              [callExpression, arguments],
                              [callExpression, Terminal "[", expression, Terminal "]"],
                              [callExpression, Terminal ".", identifier]]

arguments = Nonterminal [[Terminal "(", Terminal ")"],
                         [Terminal "(", argumentList, Terminal ")"]]


argumentList = Nonterminal [[assignmentExpression],
                            [argumentList, Terminal ",", assignmentExpression]]

leftHandSideExpression = oneOf [newExpression, callExpression]

----Postfix Expressions
postfixExpression = Nonterminal [[leftHandSideExpression],
                                 [leftHandSideExpression, Terminal "++"],
                                 [leftHandSideExpression, Terminal "--"]]

----Unary Operations
unaryExpression = Nonterminal [[postfixExpression],
                               [Terminal "delete ", unaryExpression],
                               [Terminal "void ", unaryExpression],
                               [Terminal "typeof ", unaryExpression],
                               [Terminal "++ ", unaryExpression],
                               [Terminal "-- ", unaryExpression],
                               [Terminal "+ ", unaryExpression],
                               [Terminal "- ", unaryExpression],
                               [Terminal "~ ", unaryExpression],
                               [Terminal "! ", unaryExpression]]

----Multiplicative Operators
multiplicativeExpression = Nonterminal [[unaryExpression],
                                        [multiplicativeExpression, Terminal "*", unaryExpression],
                                        [multiplicativeExpression, Terminal "/", unaryExpression],
                                        [multiplicativeExpression, Terminal "%", unaryExpression]]

----Additive Operators
additiveExpression = Nonterminal [[multiplicativeExpression],
                                  [additiveExpression, Terminal " + ", multiplicativeExpression],
                                  [additiveExpression, Terminal " - ", multiplicativeExpression]]

----Bitwise Shift Operators
shiftExpression = Nonterminal [[additiveExpression],
                               [shiftExpression, Terminal " << ", additiveExpression],
                               [shiftExpression, Terminal " >> ", additiveExpression],
                               [shiftExpression, Terminal " >>> ", additiveExpression]]

----Relational Operators
relationalExpression = Nonterminal [[shiftExpression],
                                    [relationalExpression, Terminal "<", shiftExpression],
                                    [relationalExpression, Terminal ">", shiftExpression],
                                    [relationalExpression, Terminal "<=", shiftExpression],
                                    [relationalExpression, Terminal "<=", shiftExpression],
                                    [relationalExpression, Terminal " instanceof ", shiftExpression],
                                    [relationalExpression, Terminal " in ", shiftExpression]]

relationalExpressionNoIn = Nonterminal [[shiftExpression],
                                    [relationalExpressionNoIn, Terminal "<", shiftExpression],
                                    [relationalExpressionNoIn, Terminal ">", shiftExpression],
                                    [relationalExpressionNoIn, Terminal "<=", shiftExpression],
                                    [relationalExpressionNoIn, Terminal "<=", shiftExpression],
                                    [relationalExpressionNoIn, Terminal " instanceof ", shiftExpression]]

----Equality Operators

equalityExpression = Nonterminal [[relationalExpression],
                                  [equalityExpression, Terminal "==", relationalExpression],
                                  [equalityExpression, Terminal "!=", relationalExpression],
                                  [equalityExpression, Terminal "===", relationalExpression],
                                  [equalityExpression, Terminal "!==", relationalExpression]]

equalityExpressionNoIn = Nonterminal [[relationalExpressionNoIn],
                                      [equalityExpressionNoIn, Terminal "==",  relationalExpressionNoIn],
                                      [equalityExpressionNoIn, Terminal "!=",  relationalExpressionNoIn],
                                      [equalityExpressionNoIn, Terminal "===", relationalExpressionNoIn],
                                      [equalityExpressionNoIn, Terminal "!==", relationalExpressionNoIn]]

----Binary Bitwise Operators
bitwiseANDExpression = Nonterminal [[equalityExpression],
                                    [bitwiseANDExpression, Terminal "&", equalityExpression]]

bitwiseXORExpression = Nonterminal [[bitwiseANDExpression],
                                    [bitwiseXORExpression, Terminal "^", bitwiseANDExpression]]

bitwiseORExpression = Nonterminal [[bitwiseXORExpression],
                                   [bitwiseORExpression, Terminal "|", bitwiseXORExpression]]



bitwiseANDExpressionNoIn = Nonterminal [[equalityExpressionNoIn],
                                        [bitwiseANDExpressionNoIn, Terminal "&", equalityExpressionNoIn]]

bitwiseXORExpressionNoIn = Nonterminal [[bitwiseANDExpressionNoIn],
                                        [bitwiseXORExpressionNoIn, Terminal "^", bitwiseANDExpressionNoIn]]

bitwiseORExpressionNoIn= Nonterminal [[bitwiseXORExpression],
                                      [bitwiseORExpressionNoIn, Terminal "|", bitwiseXORExpressionNoIn]]


----Binary Logical Operators
logicalANDExpression = Nonterminal [[bitwiseORExpression],
                                    [logicalANDExpression, Terminal "&&", bitwiseORExpression]]

logicalANDExpressionNoIn = Nonterminal [[bitwiseORExpressionNoIn],
                                        [logicalANDExpressionNoIn, Terminal "&&", bitwiseORExpressionNoIn]]

logicalORExpression = Nonterminal [[logicalANDExpression],
                                   [logicalORExpression, Terminal "||", logicalANDExpression]]

logicalORExpressionNoIn = Nonterminal [[logicalANDExpressionNoIn],
                                       [logicalORExpressionNoIn, Terminal "||", logicalANDExpressionNoIn]]

----Conditional Operator
conditionalExpression = Nonterminal [[logicalORExpression],
                                     [logicalORExpression, Terminal "?", assignmentExpression, Terminal ":", assignmentExpression]]


conditionalExpressionNoIn = Nonterminal [[logicalORExpressionNoIn],
                                         [logicalORExpressionNoIn, Terminal "?", assignmentExpressionNoIn, Terminal ":", assignmentExpressionNoIn]]


----Assignment Operator
assignmentExpression = Nonterminal [[conditionalExpression],
                                    [leftHandSideExpression, assignmentOperator, assignmentExpression]]

assignmentExpressionNoIn = Nonterminal [[conditionalExpressionNoIn],
                                        [leftHandSideExpression, assignmentOperator, assignmentExpressionNoIn]]
                                        
assignmentOperator = oneOf ["=","*=","/=","%=","+=","-=","<<=",">>=",">>>=","&=","^=","|="]


----Comma Operator
expression = Nonterminal [[assignmentExpression],
                          [expression, Terminal ",", assignmentExpression]]

expressionNoIn = Nonterminal [[assignmentExpressionNoIn],
                              [expressionNoIn, Terminal ",", assignmentExpressionNoIn]]

-----Statements
statement = oneOf [block,variableStatement,emptyStatement,expressionStatement,
                   ifStatement,iterationStatement,continueStatement,breakStatement,
                   returnStatement,withStatement,labelledStatement,switchStatement,
                   throwStatement,tryStatement]

----Block
block = Nonterminal [[Terminal "{",optional statementList, Terminal "}"]]
statementList = many1 statement

----Block Statement
variableStatement = Nonterminal [[Terminal " var ", variableDeclarationList, Terminal ";"]]
variableDeclarationList = Nonterminal [[variableDeclaration],
                                       [variableDeclarationList, Terminal ",", variableDeclaration]]

variableDeclarationListNoIn = Nonterminal [[variableDeclarationNoIn],
                                           [variableDeclarationListNoIn, Terminal ",", variableDeclarationNoIn]]

variableDeclaration = Nonterminal [[identifier, optional initialiser]]

variableDeclarationNoIn = Nonterminal [[identifier, optional initialiserNoIn]]

initialiser = Nonterminal [[Terminal "=", assignmentExpression]]
initialiserNoIn = Nonterminal [[Terminal "=", assignmentExpressionNoIn]]


----Empty Statement
emptyStatement = Terminal ";"

----Expression Statement

--todo: ensure that this doesn't start with a curly (or that this is no problem)
expressionStatement = Nonterminal [[expression, Terminal ";"]]


----The if Statement
ifStatement = Nonterminal [[Terminal " if (", expression, Terminal ")", statement],
                           [Terminal " if (", expression, Terminal ")", statement, Terminal " else ", statement]]


----Iteration statements
iterationStatement = Nonterminal [
    [Terminal " do ",statement, Terminal " while (", expression, Terminal ");"],
    [Terminal " while (", expression, Terminal ")", statement],
    [Terminal " for (", optional expressionNoIn, Terminal ";", optional expression, Terminal ";", optional expression, Terminal ")", statement],
    [Terminal " for (var ", variableDeclarationListNoIn, Terminal ";", optional expression, Terminal ";", optional expression, Terminal ")", statement],
    [Terminal " for (", leftHandSideExpression, Terminal " in ", expression, Terminal ")", statement],
    [Terminal " for (var", variableDeclarationNoIn, Terminal " in ", expression, Terminal ")", statement]]

----The continue Statement
continueStatement = Nonterminal [[Terminal " continue;"],
                                 [Terminal " continue ", identifier, Terminal ";"]]

----The break Statement
breakStatement = Nonterminal [[Terminal " break;"],
                              [Terminal " break ", identifier, Terminal ";"]]

----The return Statement
returnStatement = Nonterminal [[Terminal " return;"],
                               [Terminal " return ", expression, Terminal ";"]]

----The with Statement
withStatement = Nonterminal [[Terminal " with (", expression, Terminal ")", statement]]

----The switch Statement
switchStatement = Nonterminal [[Terminal " switch (", expression, Terminal ")", caseBlock]]
caseBlock = Nonterminal [[Terminal "{", Terminal "}"],
                         [Terminal "{", caseClauses, Terminal "}"],
                         [Terminal "{", caseClauses, defaultClause, Terminal "}"],
                         [Terminal "{", caseClauses, defaultClause, caseClauses, Terminal "}"]]
caseClauses = many1 caseClause
caseClause = Nonterminal [[Terminal " case ", expression, Terminal ":"],
                          [Terminal " case ", expression, Terminal ":", statementList]]

defaultClause = Nonterminal [[Terminal " default: "],
                             [Terminal " default: ", statementList]]


----Labelled Statements
labelledStatement = Nonterminal [[identifier, Terminal ":", statement]]

----The throw Statement
throwStatement = Nonterminal [[Terminal " throw ", expression, Terminal ";"]]

----The try Statement
tryStatement = Nonterminal [[Terminal " try ", block, catch],
                            [Terminal " try ", block, finally],
                            [Terminal " try ", block, catch, finally]]

catch = Nonterminal [[Terminal " catch(", identifier, Terminal ")", block]]
finally = Nonterminal [[Terminal " finally", block]]


----- Function Definition
functionDeclaration = Nonterminal [[Terminal " function ", identifier, 
                                    Terminal "(", optional formalParameterList, Terminal "){", functionBody, Terminal"}"]]

functionExpression = Nonterminal [[Terminal " function ", optional identifier, 
                                   Terminal "(", optional formalParameterList, Terminal "){", functionBody, Terminal"}"]]

formalParameterList = Nonterminal [[identifier],
                                   [formalParameterList, Terminal ",", identifier]]

functionBody = sourceElements


----- Program

program = sourceElements

sourceElements = many1 sourceElement
sourceElement = oneOf [statement, functionDeclaration]






--Executing the generator

mkcd dirname = do createDirectoryIfMissing False dirname
                  setCurrentDirectory dirname

filesPerDirectory :: Int
filesPerDirectory = 20000

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto k l = h:(splitInto k t)
        where (h,t) = splitAt k l


readArgs [s] = read s
readArgs _   = toEnum filesPerDirectory

main = do args <- getArgs
          mkcd "gen"
          mapM_ handleGroup $ zip [1..] $ splitInto filesPerDirectory $ zip [1..] $ take (readArgs args) $ getAll $ program
          

format :: Int -> String
format n = printf "%08d" n

handleGroup (n,ps) = do mkcd $ format n
                        mapM_ writeProgramToFile $ ps
                        setCurrentDirectory ".."

writeProgramToFile (n, p) = writeFile (format n ++ ".js") p