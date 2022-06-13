module FSharpLint.Rules.GenericTypesNormalization

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

let private getFirstMatchOfOpeningBracket words =
    let rec loop remainingWords accumulator =
        match remainingWords with
        | head :: rest when head = "(" ->
            rest, (head + accumulator)
        | head :: rest ->
            loop rest (head + accumulator)
        | [] -> failwith "Wrong Type format"

    loop words String.Empty

let private generateGenericStyle tokens =
    let mutable front: string = String.Empty
    let mutable heads: list<string> = list.Empty
    tokens |> List.iter (fun x ->
        if x = "*" || x = ")" || x = "(" then
            if x = "*" then
                front <- front + " * "
                heads <- front :: heads
                front <- ""
            else
                if x = "(" then
                    heads <- x :: front :: heads
                    front <- ""
                else
                    let rest, accumulator = getFirstMatchOfOpeningBracket heads
                    heads <- rest
                    front <- accumulator + front + x
        else
            if front.Length > 0 then
                front <- x + "<" + front + ">"
            else
                front <- x)
    String.Join( "", (List.toArray heads)) + front

let private tokenize source : list<string> =
    let mutable tokens: list<string> = list.Empty
    let mutable chars: list<char> = list.Empty
    source |> String.iter (fun x ->
        if x = ')' || x = '(' || x = '*' then
            if chars.Length > 0 then
                tokens <- String(chars |> List.rev |> List.toArray) :: tokens
                chars <- list.Empty
            tokens <- string x :: tokens
        elif x = ' ' then
            if chars.Length > 0 then
                tokens <- String(chars |> List.rev |> List.toArray) :: tokens
                chars <- list.Empty
        else
            chars <- x :: chars)
    if chars.Length > 0 then
        tokens <- String(chars |> List.rev |> List.toArray) :: tokens
    List.rev tokens

let private generateFix (text:string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let toText  = fromText.Trim() |> tokenize |> generateGenericStyle
        { FromText = fromText; FromRange = range; ToText = toText }))

let private generateGenericStyleForSubType tokens =
    let mutable front: string = String.Empty
    let mutable heads: list<string> = list.Empty
    let mutable isSubType = false
    tokens |> List.iter (fun x ->
        if x = "*" || x = ")" || x = "(" then
            if x = "*" then
                front <- front + " * "
                heads <- front :: heads
                front <- ""
            else
                if x = "(" then
                    heads <- x :: front :: heads
                    front <- ""
                else
                    let rest, accumulator = getFirstMatchOfOpeningBracket heads
                    heads <- rest
                    front <- accumulator + front + x
        else
            if x = ":>" then
                front <- front + " " + x + " "
                isSubType <- true
            elif front.Length > 0 && x <> "when" && not isSubType  && not (x.StartsWith("'")) then
                front <- x + "<" + front + " "
            else
                if isSubType then
                    front <- front + x + ">"
                    isSubType <- false
                else
                    if String.IsNullOrEmpty front then
                        front <- x
                    else
                        if front.EndsWith(" ") then
                            front <- front + x
                        else
                            front <- front + " " + x)
    String.Join( "", (List.toArray heads)) + front

let private generateFixwithSubType (text:string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let mutable toText  = fromText.Trim()
        let mutable inBetween = ""
        let mutable words = toText.Split('=')
        if words.Length > 0 then
            inBetween <- " ="
        let mutable tokenized = tokenize (words.[0])
        let generatedFromTokens = generateGenericStyleForSubType tokenized
        if inBetween.Length > 0 then
            toText <- generatedFromTokens + inBetween  + String.Join(" ", (Array.sub words 1 (words.Length - 1)))
        else
            toText <- generatedFromTokens + String.Join(" ", (Array.sub words 1 (words.Length - 1)))

        { FromText = fromText; FromRange = range; ToText = toText }))

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Type(SynType.App(SynType.LongIdent (LongIdentWithDots ([_typ], [])), None, _types, _, _, _, range)) ->
        { Range = range
          Message = Resources.GetString("RulesGenericTypesNormalizationError")
          SuggestedFix = Some (generateFix args.FileContent range)
          TypeChecks = List.Empty }
        |> Array.singleton
    | AstNode.TypeDefinition(SynTypeDefn(SynComponentInfo(_, [_typeDec], _, _, _, false, _, _), _, _, _, range)) ->
        { Range = range
          Message = Resources.GetString("RulesGenericTypesNormalizationError")
          SuggestedFix = Some (generateFixwithSubType args.FileContent range)
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
