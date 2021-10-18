open ClearFactor.Parser
open FParsec.CharParsers
[<EntryPointAttribute>]
let main _ =
    
    """
    : @sum { @list @acc } @list @elem loop @list + @acc = @acc endLoop return @acc endReturn ;
    """
    |> parse
    |> function
       | ParserResult.Success (x, _, _) -> printfn "%A" x
       | ParserResult.Failure (x, _, _) -> printfn "%A" x
    
    0
