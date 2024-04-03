// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. (spaces >>. p2)
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = p1 >>. (spaces >>. p2)

    let parenthesise p =  between ( pchar '(' .>> spaces) (spaces >>. pchar ')') p

    let charListToStr charList = System.String(List.toArray charList) |> string
    let pid =
        let initialChar = pletter <|> pchar '_'
        let subsequentChars = many (palphanumeric <|> pchar '_')
        let identifier = initialChar .>>. subsequentChars |>> (fun (first, rest) -> first :: rest)
                        |>> charListToStr
        identifier <?> "identifier"

    
    let unop op p = op >*>. p

    let binop op a b = (a .>> spaces) .>> op .>>. (spaces >>. b) 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; ModParse; DivParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> fun (x) -> Mul((N -1), x)
    let PVParse = unop (pPointValue) AtomParse |>> PV <?> "PV"
    let VarParse = pid |>> V <?> "Var"

    let AexpParse = TermParse 
    let CParse = between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "Char"
    let ToUpperParse = unop (pToUpper) (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop (pToLower) (parenthesise CharParse) |>> ToLower <?> "ToLower"
    let CVParse = unop (pCharValue) (parenthesise AtomParse) |>> CV <?> "CV"
    let IntToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"

    do cref := choice [CVParse; IntToCharParse; ToUpperParse; ToLowerParse; CParse]

    let CharToIntParse = unop (pCharToInt) (parenthesise CharParse) |>> CharToInt <?> "CharToInt"

    do aref := choice [CharToIntParse; NegParse; PVParse; VarParse; NParse; ParParse]

    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
