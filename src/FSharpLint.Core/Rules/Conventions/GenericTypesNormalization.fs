module FSharpLint.Rules.GenericTypesNormalization

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion


let private generateGenericStyle tokens =
    let getFirstMatchOfClosingBracket words =
        let rec loop remainingWords accumulator =
            match remainingWords with
            | head :: rest when head = "(" ->
                rest, (head + accumulator)
            | head :: rest ->
                loop rest (head + accumulator)
            | [] -> failwith "Wrong Type format"

        loop words String.Empty

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
                    let rest, accumulator = getFirstMatchOfClosingBracket heads
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
        tokens <- (String(chars |> List.rev |> List.toArray)) :: tokens
    List.rev tokens

let private generateFix (text:string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let toText  = fromText.Trim() |> tokenize |> generateGenericStyle
        { FromText = fromText; FromRange = range; ToText = toText }))

let private runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Type(SynType.App(SynType.LongIdent (LongIdentWithDots ([_typ], [])), None, _types, _, _, _, range)) ->
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
