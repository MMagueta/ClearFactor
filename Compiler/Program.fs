open Forth.Parser

[<EntryPointAttribute>]
let main _ =
    """
         [ @list @acc ] @sum let
         list @elem loop
         @list + @acc = @acc
         endLoop
         @acc return
         endLet
    """
    |> (fun x -> x.Split(" "))
    |> parse
    |> printfn "%A"
    0
