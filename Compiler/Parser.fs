namespace Forth

module Parser =
    open FParsec
    
    type ForthLangTypes =
        | FString of string
        | FInt of int
        | FFloat of float32
        | FAtom of string
        | FBool of bool
        | FList of ForthLangTypes list
        | FLabel of string
    
    type ForthParser<'T> = Parser<'T, unit>
    
    let parseExpression, parseExpressionRef: ForthParser<ForthLangTypes> * ForthParser<ForthLangTypes> ref = createParserForwardedToRef ()

    let chr c = skipChar c
    let endBy p sep = many (p .>> sep)
    let symbol: ForthParser<char> = anyOf "!=<>+*/^%&|?#@"

    let parseInt: ForthParser<ForthLangTypes> = spaces >>. pint32 .>> spaces |>> FInt
    let parseString: ForthParser<ForthLangTypes> =
        parse {
            do! chr '"'
            let! xs = manyChars (noneOf "\"") .>> spaces
            do! chr '"'
            return FString xs
        }
        .>> spaces
    let parseList: ForthParser<ForthLangTypes> =
        between (pchar '[' .>> spaces) (pchar ']' .>> spaces) (many parseExpression)
        |>> FList
    let parseBool: ForthParser<ForthLangTypes> =
        (stringReturn "#T"  true)
        <|> (stringReturn "#F" false)
        |>> FBool
    let parseAtom: ForthParser<ForthLangTypes> =
        (stringReturn "let"  "let")
        <|> (stringReturn "endLet" "endLet")
        <|> (stringReturn "endLoop" "endLoop")
        <|> (stringReturn "loop" "loop")
        <|> (stringReturn "return" "return")
        |>> FAtom
        
    let parseLabel: ForthParser<ForthLangTypes> =
        parse {
            do! chr '@'
            let! xs = manyChars (noneOf "\"") .>> spaces
            return FLabel xs
        }
    
    let runParserRef () =
        do
            (parseExpressionRef
             := choice [ parseInt
                         parseString
                         parseBool
                         parseList
                         parseAtom
                         parseLabel ])

    let expressions (input: string) = run (spaces >>. many parseExpression) input

    let parse (input: string array) =
        runParserRef ()
        expressions (System.String.Join(" ", input))
