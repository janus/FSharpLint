module FSharpLint.Rules.GenericTypesNormalization

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let generateGenericStyle tokens =
    let mutable front: string = String.Empty
    let mutable back: string = String.Empty
    let mutable heads: list<string> = list.Empty
    let mutable tails: list<string> = list.Empty
    let mutable consecutive = false
    let mutable isBracketOpening = false
    let mutable first = true
    let mutable temp: string = String.Empty
    tokens |> List.iter (fun x ->
        if x = "*" || x = ")" || x = "(" || x.Length = 1 then
            if x = "*" then
                front <- front + " * "
                consecutive <- false
            else
                if x = "(" then
                    heads <- front :: heads
                    tails <- back :: tails
                    front <- "<("
                    back <- ""
                    isBracketOpening <- true
                elif x = ")" then
                    front <- (heads.Head) + front
                    back <- back + temp + ")>" + (tails.Head)
                    temp <- ""
                    consecutive <- false
                else
                    front <- front + x
                    consecutive <- false
        else
            if isBracketOpening then
                front <- front + x
                isBracketOpening <- false
                //consecutive <- true
            elif consecutive then
                front <- front + temp + "<" + x
                back <- ">" + back
                temp <- ""
                consecutive <- false
            else
                if first then
                    first <- false
                    front <- front + x
                else
                    temp <- x
                consecutive <- true)
    front + back

let tokenize source : list<string> =
    let mutable tokens: list<string> = list.Empty
    let mutable chars: list<char> = list.Empty
    source |> String.iter (fun x ->
        if x = ')' || x = '(' || x = '*' then
            if chars.Length > 0 then
                tokens <- (String(chars |> List.rev |> List.toArray)) :: tokens
                chars <- list.Empty
            tokens <- (string x) :: tokens
        elif x = ' ' then
            if chars.Length > 0 then
                tokens <- (String(chars |> List.rev |> List.toArray)) :: tokens
                chars <- list.Empty
        else
            chars <- x :: chars)
    if chars.Length > 0 then
        tokens <- (String(chars |> List.rev |> List.toArray)) :: (List.rev tokens)
    tokens

let private generateFix (text:string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let toText  = fromText.Trim() |> tokenize |> generateGenericStyle

        { FromText = fromText; FromRange = range; ToText = toText }))

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Type(SynType.App(SynType.LongIdent (LongIdentWithDots ([_typ], [])), None, _types, _, _, _, range)) ->
        ExpressionUtilities.tryFindTextOfRange range args.FileContent
        |> Option.map (fun fromText ->
            let mwords = fromText.Split(' ')
            printfn "%A" mwords) |> ignore
        { Range = range
          Message = Resources.GetString("RulesGenericTypesNormalizationError")
          SuggestedFix = Some (generateFix args.FileContent range)
          TypeChecks = List.Empty }
        |> Array.singleton
    | AstNode.TypeDefinition(SynTypeDefn(SynComponentInfo(_, [_typeDec], _, _, _, false, _, range), _, _, _, _)) ->
        { Range = range
          Message = Resources.GetString("RulesGenericTypesNormalizationError")
          SuggestedFix = Some (generateFix args.FileContent range)
          TypeChecks = List.Empty }
        |> Array.singleton
    | _ -> Array.empty

let rule =
    { Name = "GenericTypesNormalization"
      Identifier = Identifiers.GenericTypesNormalization
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
