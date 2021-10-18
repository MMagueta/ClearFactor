namespace ClearFactor

module Parser =
    open FParsec

    type ClearFactorLangTypes =
        | CFString of string
        | CFInt of int
        | CFFloat of float32
        | CFAtom of string * ClearFactorLangTypes list
        | CFBool of bool
        | CFList of ClearFactorLangTypes list
        | CFParams of ClearFactorLangTypes list
        | CFLabel of string
        | CFScope of ClearFactorLangTypes list

    type ClearFactorParser<'T> = Parser<'T, unit>

    let parseExpression, parseExpressionRef: ClearFactorParser<ClearFactorLangTypes> * ClearFactorParser<ClearFactorLangTypes> ref =
        createParserForwardedToRef ()

    let chr c = skipChar c
    let endBy p sep = many (p .>> sep)
    let symbol: ClearFactorParser<char> = anyOf "!=<>+*/^%&|?#"

    let parseInt: ClearFactorParser<ClearFactorLangTypes> = spaces >>. pint32 .>> spaces |>> CFInt

    let parseString: ClearFactorParser<ClearFactorLangTypes> =
        parse {
            do! chr '"'
            let! xs = manyChars (noneOf "\"") .>> spaces
            do! chr '"'
            return CFString xs
        }
        .>> spaces

    let parseList: ClearFactorParser<ClearFactorLangTypes> =
        between (pchar '[' .>> spaces) (pchar ']' .>> spaces) (many parseExpression)
        |>> CFList
        
    let parseScope: ClearFactorParser<ClearFactorLangTypes> =
        between (pchar ':' .>> spaces) (pchar ';' .>> spaces) (many parseExpression)
        |>> CFScope
        
    let parseParams: ClearFactorParser<ClearFactorLangTypes> =
        between (pchar '{' .>> spaces) (pchar '}' .>> spaces) (many parseExpression)
        |>> CFParams

    let parseBool: ClearFactorParser<ClearFactorLangTypes> =
        (stringReturn "#T" true)
        <|> (stringReturn "#F" false)
        |>> CFBool

    let parseAtom: ClearFactorParser<ClearFactorLangTypes> =
        parse {
            let! atom = pstring "loop"
            let! sequence = between (spaces1) (pstring "endLoop" .>> spaces) (many parseExpression)
            return (CFAtom (atom, sequence))
        }
        <|>
        parse {
            let! atom = pstring "return"
            let! sequence = between (spaces1) (pstring "endReturn" .>> spaces) (many parseExpression)
            return (CFAtom (atom, sequence))
        }

    let parseLabel: ClearFactorParser<ClearFactorLangTypes> =
        parse {
            let! label = chr '@' >>. (many1Chars (letter <|> digit <|> symbol)) .>> spaces
            return (CFLabel ("@" + label))
        }

    let parseSymbols: ClearFactorParser<ClearFactorLangTypes> =
        parse {
            let! label = spaces >>. (many1Chars symbol) .>> spaces
            return (CFLabel label)
        }
    
    let runParserRef () =
        do
            (parseExpressionRef
             := choice [ parseLabel
                         parseInt
                         parseString
                         parseBool
                         parseList
                         parseParams
                         parseScope
                         parseAtom
                         parseSymbols ])

    let expressions (input: string) =
        run (many1 (spaces >>. parseExpression)) input

    let parse (input: string) =
        runParserRef ()
        //expressions (System.String.Join(" ", input))
        expressions input
