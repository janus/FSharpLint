module FSharpLint.Rules.GenericTypesNormalization

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

let private getFirstMatchOfOpeningBracket words =
    let rec loop remainingWords accumulator =
        match remainingWords with
        | head :: tail when head = "(" ->
            tail, (head + accumulator)
        | head :: tail ->
            loop tail (head + accumulator)
        | [] -> failwith "Wrong Type format"

    loop words String.Empty

let private generateGenericStyle tokens =
    let rec loop tokens (front: string) (heads: List<string>) =
        match tokens with
        | "*" :: tail -> loop tail String.Empty (front + " * " :: heads)
        | "(" :: tail -> loop tail String.Empty ("(" :: front :: heads)
        | ")" :: tail ->
            let others, accumulator = getFirstMatchOfOpeningBracket heads
            loop tail (accumulator + front + ")") others
        | head :: tail when front.Length > 0 -> loop tail (head + "<" + front + ">") heads
        | head :: tail -> loop tail head heads
        | [] -> String.Join(String.Empty, List.toArray heads) + front

    loop tokens String.Empty List.Empty

let private tokenize (source: string) : List<string> =
    let chars = Array.toList(source.ToCharArray())
    let rec loop (chars: List<char>) (accumulator: List<char>) (tokens: List<string>) =
        match chars with
        | head :: tail when "()*".Contains head ->
            if accumulator.Length > 0 then
                let accumulated = String(accumulator |> List.rev |> List.toArray)
                loop tail List.Empty (string head :: accumulated :: tokens)
            else
                loop tail accumulator (string head :: tokens)
        | head :: tail when head = ' ' ->
            if accumulator.Length > 0 then
                loop tail List.Empty (String(accumulator |> List.rev |> List.toArray) :: tokens)
            else
                loop tail accumulator tokens
        | head :: tail -> loop tail (head :: accumulator) tokens
        | [] ->
            if accumulator.Length > 0 then
                List.rev (String(accumulator |> List.rev |> List.toArray) :: tokens)
            else
                List.rev tokens

    loop chars List.Empty List.Empty

let private generateFix (text: string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let toText  = fromText.Trim() |> tokenize |> generateGenericStyle
        { FromText = fromText; FromRange = range; ToText = toText }))

let private getType attributes text typeMatch =
    let rec loop (remainingAttributes: List<SynAttribute>) =
        match remainingAttributes with
        | head :: tail ->
            match head.ArgExpr with
            | SynExpr.Paren(SynExpr.Tuple(_, [SynExpr.TypeApp(_, _, [SynType.App(SynType.LongIdent (LongIdentWithDots ([_type], [])), None, _types, _, _, _, range)], _, _, _, _); _], _, _), _, _, _) ->
                { Range = range
                  Message = Resources.GetString "RulesGenericTypesNormalizationError"
                  SuggestedFix = generateFix text range |> Some
                  TypeChecks = (fun () -> typeMatch) |> List.singleton }
                |> Array.singleton
            | _ -> loop tail
        | [] -> Array.empty

    loop attributes

let rec private generateGenericStyleForSubType tokens front generated isSubType closingCount =
    match tokens with
    | "*" :: tail ->
        generateGenericStyleForSubType tail String.Empty (front + "*" :: generated) isSubType closingCount
    | "(" :: tail ->
        generateGenericStyleForSubType tail String.Empty ("(" :: front :: generated) isSubType closingCount
    | ")" :: tail ->
        let pending, accumulator = getFirstMatchOfOpeningBracket generated
        generateGenericStyleForSubType tail (accumulator + front + ")") pending isSubType closingCount
    | ":>" :: tail ->
        generateGenericStyleForSubType tail (front + " :> ") generated true closingCount
    | head :: tail when front.Length > 0 && head <> "when" && not isSubType  && not (head.StartsWith "'") ->
        generateGenericStyleForSubType tail (head + "<" + front + " ") generated isSubType (closingCount + 1)
    | head :: tail when isSubType ->
        generateGenericStyleForSubType tail (front + head + ">") generated false (closingCount - 1)
    | head :: tail ->
        if String.IsNullOrEmpty front then
            generateGenericStyleForSubType tail head generated isSubType closingCount
        else
            if front.EndsWith " " then
                generateGenericStyleForSubType tail (front + head) generated isSubType closingCount
            else
                generateGenericStyleForSubType tail (front + " " + head) generated isSubType closingCount
    | [] ->
        if closingCount > 0 then
            String.Join(String.Empty, List.toArray generated) + front.TrimEnd() + ">"
        else
            String.Join(String.Empty, List.toArray generated) + front

let private generateFixWithSubType (text: string) range = lazy(
    ExpressionUtilities.tryFindTextOfRange range text
    |> Option.map (fun fromText ->
        let words = fromText.Trim().Split('=')
        let generatedFromTokens = generateGenericStyleForSubType (words.[0] |> tokenize) String.Empty List.Empty false 0
        if words.Length > 0 then
            let toText = generatedFromTokens + " ="  + String.Join(" ", Array.sub words 1 (words.Length - 1))
            { FromText = fromText; FromRange = range; ToText = toText }
        else
            let toText = generatedFromTokens + String.Join(" ", Array.sub words 1 (words.Length - 1))
            { FromText = fromText; FromRange = range; ToText = toText }))

let private getWarningDetails text range (checkFile: FSharpCheckFileResults) isSubType =
    let getWarningDetails isSub typeMatch =
        { Range = range
          Message = Resources.GetString "RulesGenericTypesNormalizationError"
          SuggestedFix = if isSub then
                            generateFixWithSubType text range |> Some
                         else
                            generateFix text range |> Some
          TypeChecks = (fun () -> typeMatch) |> List.singleton }
        |> Array.singleton

    let assemblySignature = checkFile.PartialAssemblySignature
    if assemblySignature.Entities.Count > 0 then
        let moduleEnt = assemblySignature.Entities.[0]
        let hasMeasure (typeText: string) =
            let isMeasureEntity (entity: FSharpEntity) =
                typeText.Contains(entity.ToString()) && entity.IsMeasure
            Seq.exists isMeasureEntity moduleEnt.NestedEntities
        match ExpressionUtilities.tryFindTextOfRange range text with
        | Some typeText when hasMeasure typeText -> Array.empty
        | _ -> getWarningDetails isSubType true
    else
        getWarningDetails isSubType false

let private runner (args: AstNodeRuleParams) =
    match args.AstNode, args.CheckInfo with
    | AstNode.Type(SynType.App(SynType.LongIdent (LongIdentWithDots ([_type], [])), None, _types, _, _, _, range)), Some checkFile ->
        getWarningDetails args.FileContent range checkFile false
    | AstNode.TypeDefinition(SynTypeDefn(SynComponentInfo(_, [_typeDec], _, _, _, false, _, _), _, _, _, range)), Some checkFile ->
        getWarningDetails args.FileContent range checkFile true
    | AstNode.Binding(SynBinding(_, _, _, _, [attributes], _, _, _, _, _, _, _)), _ ->
        getType attributes.Attributes args.FileContent true
    | _ -> Array.empty

let rule =
    { Name = "GenericTypesNormalization"
      Identifier = Identifiers.GenericTypesNormalization
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
