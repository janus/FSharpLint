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

let getType attributes text =
    let rec loop (remainingAttributes: list<SynAttribute>) =
        match remainingAttributes with
        | head :: rest ->
            match head.ArgExpr with
            | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.TypeApp(_, _, [SynType.App(SynType.LongIdent (LongIdentWithDots ([_typ], [])), None, _types, _, _, _, range)], _, _, _, _); _], _, _), _, _, _) ->
                { Range = range
                  Message = Resources.GetString("RulesGenericTypesNormalizationError")
                  SuggestedFix = Some (generateFix text range)
                  TypeChecks = List.Empty }
                |> Array.singleton
            | _ -> loop rest
        | [] -> Array.empty
    loop attributes

let rec private generateGenericStyleForSubType tokens front generated isSubType closingCount =
    match tokens with
    | "*" :: rest ->
        generateGenericStyleForSubType rest "" (front + "*" :: generated) isSubType closingCount
    | "(" :: rest ->
        generateGenericStyleForSubType rest "" ("(" :: front :: generated) isSubType closingCount
    | ")" :: rest ->
        let pending, accumulator = getFirstMatchOfOpeningBracket generated
        generateGenericStyleForSubType rest (accumulator + front + ")") pending isSubType closingCount
    | ":>" :: rest ->
        generateGenericStyleForSubType rest (front + " :> ") generated true closingCount
    | head :: rest when front.Length > 0 && head <> "when" && not isSubType  && not (head.StartsWith "'") ->
        generateGenericStyleForSubType rest (head + "<" + front + " ") generated isSubType (closingCount + 1)
    | head :: rest when isSubType ->
        generateGenericStyleForSubType rest (front + head + ">") generated false (closingCount - 1)
    | head :: rest ->
        if String.IsNullOrEmpty front then
            generateGenericStyleForSubType rest head generated isSubType closingCount
        else
            if front.EndsWith " " then
                generateGenericStyleForSubType rest (front + head) generated isSubType closingCount
            else
                generateGenericStyleForSubType rest (front + " " + head) generated isSubType closingCount
    | [] ->
        if closingCount > 0 then
            String.Join("", List.toArray generated) + front.TrimEnd() + ">"
        else
            String.Join("", List.toArray generated) + front

let private generateFixwithSubType (text:string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let words = fromText.Trim().Split('=')
        let generatedFromTokens = generateGenericStyleForSubType (words.[0] |> tokenize) (String.Empty) (list.Empty) false 0
        if words.Length > 0 then
            let toText = generatedFromTokens + " ="  + String.Join(" ", Array.sub words 1 (words.Length - 1))
            { FromText = fromText; FromRange = range; ToText = toText }
        else
            let toText = generatedFromTokens + String.Join(" ", Array.sub words 1 (words.Length - 1))
            { FromText = fromText; FromRange = range; ToText = toText }))

let private isAttributeMeasure (attributes: list<SynAttribute>) =
    let rec loop (identifiers: list<Ident>) =
        match identifiers with
        | head :: _ when head.idText = "Measure" ->
            true
        | _ :: rest ->
            loop rest
        | [] -> false

    match attributes with
    | head :: _ ->
        match head.TypeName with
        | LongIdentWithDots (identifiers, [])  ->
            loop identifiers
        | _ -> false
    | _ -> false

let private runner (args: AstNodeRuleParams) =
    match (args.AstNode, args.CheckInfo) with
    | (AstNode.Type(SynType.App(SynType.LongIdent (LongIdentWithDots ([_typ], [])), None, _types, _, _, _, range)), _) ->
        match (List.rev (args.GetParents 3)).[0] with
        | AstNode.TypeDefinition(SynTypeDefn(SynComponentInfo([attributes], _, _, _, _, _, _, _), _, _, _, _)) when isAttributeMeasure attributes.Attributes ->
            Array.empty
        | _ ->
            { Range = range
              Message = Resources.GetString("RulesGenericTypesNormalizationError")
              SuggestedFix = Some (generateFix args.FileContent range)
              TypeChecks = List.Empty }
            |> Array.singleton
    | (AstNode.TypeDefinition(SynTypeDefn(SynComponentInfo(_, [_typeDec], _, _, _, false, _, _), _, _, _, range)), _) ->
        { Range = range
          Message = Resources.GetString("RulesGenericTypesNormalizationError")
          SuggestedFix = Some (generateFixwithSubType args.FileContent range)
          TypeChecks = List.Empty }
        |> Array.singleton
    | (AstNode.Binding(SynBinding(_, _, _, _, [attributes], _, _, _, _, _, _, _)), _) ->
        getType (attributes.Attributes) args.FileContent
    | _ -> Array.empty

let rule =
    { Name = "GenericTypesNormalization"
      Identifier = Identifiers.GenericTypesNormalization
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
