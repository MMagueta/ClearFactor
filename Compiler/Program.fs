open Forth.Parser
open FParsec.CharParsers
[<EntryPointAttribute>]
let main _ =
    
    """
    : @sum { @list @acc } @list @elem loop @list + @acc = @acc endLoop @acc return ;
    """
    |> parse
    |> function
       | ParserResult.Success (x, y, z) -> printfn "%A" x
       | ParserResult.Failure (x, y, z) -> printfn "%A" x
    
    0
