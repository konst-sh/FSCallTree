open FParsec

module Utilities =
    let inline (<!>)<'Res, 'State when 'State:(member toShortString: unit -> string)> (p: Parser<'Res,'State>) (label: string) : Parser<'Res,'State> =
        fun stream ->
            printfn $"{stream.Position}: Entering '{label}' with UserState = {stream.UserState.toShortString()}"
            let reply = p stream
            printfn $"{stream.Position}: Leaving '{label}' ({reply.Status}) with UserState = {stream.UserState.toShortString()}"   
            reply
    
    ///Similar to bind operator, but on par with parser result provides access to starting position of the parser
    let (>>>=) (p: Parser<'a,'u>) (f: 'a -> Position -> Parser<'b,'u>): Parser<'b, 'u> =
        fun stream ->
            let position = stream.Position
            let reply1 = p stream
            if reply1.Status = Ok then
                let p2 = f reply1.Result position
                let stateTag = stream.StateTag
                let mutable reply2 = p2 stream
                if stateTag = stream.StateTag then
                    reply2.Error <- mergeErrors reply1.Error reply2.Error
                reply2
            else
                Reply(reply1.Status, reply1.Error)

    let printReturn label p =
        p >>= fun content -> 
            printfn $"Result from parser {label} = {content}"; 
            preturn content
            
    let effect (eff: unit -> unit): Parser<unit, 'TState> =
        fun stream -> eff(); Reply(())
    
    let effectSatisfies (eff: unit -> bool): Parser<unit, 'TState> =
        fun stream ->
            let status = if eff() then Ok else Error
            Reply(status, (), NoErrorMessages)
    
    let betweenStrings (openStr: string) (closeStr: string) =
        let mutable s = 0
        let pOpenScope = pstring openStr .>> effect (fun _ -> s <- s + 1)
        let pCloseScope = pstring closeStr .>> effect(fun _ -> s <- s - 1)
        let pScopeText = attempt pOpenScope <|> attempt pCloseScope <|> anyString 1
        let isOuterScope = effectSatisfies(fun () -> s = 1) <?> $"Scope = {s}"
    
        pOpenScope >>. manyTill pScopeText (isOuterScope .>> pCloseScope) |>> String.concat ""
    
    let skipBetweenStrings (openStr: string) (closeStr: string) =
        let mutable s = 0
        let pOpenScope = pstring openStr .>> effect (fun _ -> s <- s + 1)
        let pCloseScope = pstring closeStr .>> effect(fun _ -> s <- s - 1)
        let pScopeText = attempt pOpenScope <|> attempt pCloseScope <|> (skipAnyChar |>> string)
        let isOuterScope = effectSatisfies(fun _ -> s = 1) <?> $"Scope = {s}"
    
        pOpenScope >>. skipManyTill pScopeText (isOuterScope .>> pCloseScope)

    let loopParser (p: Parser<unit, 'State>): Parser<unit, 'State> =
        fun stream ->
            let mutable reply = Reply ()
            while not stream.IsEndOfStream && (reply.Status <> FatalError || reply.Status <> Error) do
                reply <- p stream
    
            reply

module Parse =
    open Utilities

    type FData = {
        Scope: int
        Name: string
        Position: Position
    }
    type State = {    
        CurrentScope: int 
        CurrentScopeOnCallStack: int list
        CallScope: int
        FDataList: FData list
    }
    with
        static member init = {
            FDataList = []
            CallScope = 0
            CurrentScopeOnCallStack = []
            CurrentScope = 0
        }

        member state.CallScopeOpen(name: string, position: Position): State =
            let fData = {
                Name = name
                Position = position
                Scope = state.CallScope
            }
            { state with
                CurrentScope = state.CurrentScope + 1
                CurrentScopeOnCallStack = state.CurrentScope :: state.CurrentScopeOnCallStack
                CallScope = state.CallScope + 1
                FDataList = fData :: state.FDataList
            }

        member state.NonCallScopeOpen(): State =
            { state with
                CurrentScope = state.CurrentScope + 1
            }

        member state.ScopeClose(): State =
            let scopeOnCurrentCall, restOfStack =
                match state.CurrentScopeOnCallStack with
                | [] -> -1, []
                | h::tl -> h, tl

            if state.CurrentScope - 1 > scopeOnCurrentCall
            then // stil in call scope - decrementing current scope
                {state with
                    CurrentScope = state.CurrentScope - 1
                }
            else //exiting call scope
                {state with
                    CurrentScope = state.CurrentScope - 1
                    CurrentScopeOnCallStack = restOfStack
                    CallScope = state.CallScope - 1
                }

        member this.toShortString() = {|
                CurrentScopeOnCallStack = this.CurrentScopeOnCallStack
                CurrentScope = this.CurrentScope |}.ToString()

    let callScopeOpenHandle (name: string) (pos: Position) (stream: CharStream<State>) =
        let state = stream.UserState
        stream.UserState <- state.CallScopeOpen(name, pos)

    let nonCallScopeOpenHandle (stream: CharStream<State>) =
        let state = stream.UserState
        stream.UserState <- state.NonCallScopeOpen()

    let closeScopeHandle (stream: CharStream<State>) =
        let state = stream.UserState
        stream.UserState <- state.ScopeClose()
    
    let isReserved name = 
        ["import"; "for"; "if"; "while"; "try"]
        |> Set.ofList 
        |> Set.contains name
    
    let ident: Parser<string, State> =
        let isAsciiIdStart c = isAsciiLetter c || c = '_'
        let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_'
        identifier <| IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue) 
        
    let functionCallOpen: Parser<unit, State> =
        ident .>> spaces .>> skipChar '('
        >>>= fun name pos stream ->
            if isReserved name 
            then nonCallScopeOpenHandle stream
            else callScopeOpenHandle name pos stream
                
            Reply(Ok, (), NoErrorMessages)
        
    let nonCallOpen: Parser<unit, State> =
        skipChar '(' <|> skipChar '{'
        >>= fun _ stream ->
            nonCallScopeOpenHandle stream
            Reply(Ok, (), NoErrorMessages)
    
    let parseClose: Parser<unit, State> =
        skipChar ')' <|> skipChar '}'
        >>= fun _ stream -> 
            closeScopeHandle stream
            Reply(Ok, (), NoErrorMessages)
    
    let anonFunctionOpen: Parser<unit, State> =
        (ident .>> spaces .>> skipChar '=' .>> spaces) |> attempt |> opt
        .>> skipString "function" .>> spaces .>>. betweenStrings "(" ")" .>> spaces
        .>>. opt (skipString "returns" >>. spaces >>. ident) .>> spaces
        .>> optional (skipString "precondition" .>> spaces .>> betweenStrings "{" "}") .>> spaces
        .>> skipChar '{'
        >>>= fun ((name, args), retType) pos stream ->
            let name = 
                let ret = if retType.IsSome then "returns " + retType.Value else ""
                let name = if name.IsSome then $"{name.Value} = " else ""
                name + $"function({args})" + ret
            callScopeOpenHandle name pos stream
            Reply(Ok, (), NoErrorMessages)
    
    let functionDeclarationOpen: Parser<unit, State> =
        skipString "function" >>. spaces >>. ident .>> spaces .>>. betweenStrings "(" ")" .>> spaces
        .>>. opt (skipString "returns" >>. spaces >>. ident) .>> spaces
        .>> optional (skipString "precondition" .>> spaces .>> betweenStrings "{" "}") .>> spaces
        .>> skipChar '{'
        >>>= fun ((name, args), retType) pos stream ->
            let name = 
                let ret = if retType.IsSome then " returns " + retType.Value else ""
                $"{name}({args})" + ret
            callScopeOpenHandle name pos stream
            Reply(Ok, (), NoErrorMessages)
    
    let defineFeatureOpen: Parser<unit, State> =
        ident .>> spaces .>> skipChar '=' .>> spaces .>> skipString "defineFeature" .>> spaces .>> skipChar '('
        >>>= fun name pos stream ->
            let name = $"{name} = defineFeature"
            callScopeOpenHandle name pos stream
            Reply(Ok, (), NoErrorMessages)

    let definePredicateOpen: Parser<unit, State> =
        skipString "predicate" >>. spaces >>. ident .>> spaces .>>. betweenStrings "(" ")" .>> spaces .>> skipChar '{'
        >>>= fun (name, args) pos stream ->
            let name = $"predicate {name}({args})"
            callScopeOpenHandle name pos stream
            Reply(Ok, (), NoErrorMessages)

    let skipComment: Parser<unit, State> =
        choice [
            skipBetweenStrings "/*" "*/"
            skipString "//".>> skipRestOfLine true
        ]

    let mainParser =
        let choiceParser = choice [
            attempt skipComment
            attempt definePredicateOpen
            attempt defineFeatureOpen
            attempt functionDeclarationOpen
            attempt anonFunctionOpen
            attempt functionCallOpen
            attempt nonCallOpen
            attempt parseClose
            skipAnyChar
        ]
    
        skipManyTill choiceParser eof

    let parseFile path =
        runParserOnFile mainParser State.init path (System.Text.Encoding.Default)

    let parseFromFile path =
        let text = System.IO.File.ReadAllText(path)
        runParserOnString mainParser State.init "" text

module InputOutput =
    open Parse

    let extractCallScopeData: ParserResult<unit, State> -> string array =
        let space (n: int) = System.String(' ', n)
        let alignTo (n: int) (s: string) = if s.Length < n then s + space (n - s.Length) else s
    
        function
        | Failure (reason, _, _) -> 
            failwith reason
        | Success (_, state, _) -> 
             state.FDataList |> List.toArray |> Array.rev
             |> Array.map(fun data ->
                let line = data.Position.Line |> string |> alignTo 8
                let column = data.Position.Column |> string |> alignTo 8
                let scope = data.Scope |> string |> alignTo 5
                let shift = space (4 * (data.Scope + 1))
    
                $"{line}{column}{scope}{shift}|{data.Name}"
             )

    let path = 
        System.AppDomain.CurrentDomain.BaseDirectory

    let printResults parserResult = 
        let header = $"Line    Col     Scope    Name"
        printfn "%s" header
        parserResult
        |> extractCallScopeData
        |> Array.iter (printfn "%s")
    
    let saveResult path parserResult  =    
        let lines = [|
            $"Line    Col     Scope    Name"
            yield! extractCallScopeData parserResult
            |]
        System.IO.File.WriteAllLines(path, lines)

    let saveCallTrees() =
        let directoryPath = System.IO.Path.Combine(path, "CallTrees")
        System.IO.Directory.CreateDirectory(directoryPath) |> ignore

        System.IO.DirectoryInfo(path).GetFiles("*.txt")
        |> Seq.toArray
        |> Array.map(fun file -> async {
            let name = System.IO.Path.GetFileNameWithoutExtension(file.Name)
            let path = System.IO.Path.Combine(directoryPath, $"{name}_call_tree.txt")
            System.Console.WriteLine($"Processing {file.Name}")
            try
                parseFromFile file.FullName
                |> saveResult path
                System.Console.WriteLine($"{file.Name} processed successfully")
            with e -> 
                System.Console.WriteLine($"Parsing {file.Name} failed: {e.Message}")
        })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

[<EntryPoint>]
let main _ =
    InputOutput.saveCallTrees()
    System.Console.WriteLine("All operations completed")
    System.Console.ReadKey()|> ignore
    0