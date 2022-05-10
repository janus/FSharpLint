module FSharpLint.Rules.PublicValuesNames

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.AstInfo
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming

let private getValueOrFunctionIdents typeChecker isPublic pattern =
    let checkNotUnionCase ident = fun () -> 
        typeChecker
        |> Option.map (fun checker -> isNotUnionCase checker ident)
        |> Option.defaultValue false

    let isNotActivePattern (ident:Ident) =
        ident.idText.StartsWith("|")
        |> not

    match pattern with
    | SynPat.LongIdent(longIdent, _, _, _, _, _) ->
        // If a pattern identifier is made up of more than one part then it's not binding a new value.
        let singleIdentifier = List.length longIdent.Lid = 1

        match List.tryLast longIdent.Lid with
        | Some ident when singleIdentifier ->
            let checkNotUnionCase = checkNotUnionCase ident
            if isPublic = Accessibility.Public && isNotActivePattern ident then
                (ident, ident.idText, Some checkNotUnionCase)
                |> Array.singleton
            else
                Array.empty
        | None | Some _ -> Array.empty
    | SynPat.Named(_, identifier, _, _, _)  when isPublic = Accessibility.Public ->
        (identifier, identifier.idText, None)
        |> Array.singleton
    | _ -> Array.empty

let private getIdentifiers (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Expression(SynExpr.ForEach(_, _, true, pattern, _, _, _)) ->
        getPatternIdents Accessibility.Private (getValueOrFunctionIdents args.CheckInfo) false pattern
    | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, valData, pattern, _, _, _, _)) ->
        if not (isLiteral attributes || isExtern attributes) then
            match identifierTypeFromValData valData with
            | Value | Function ->
                let accessibility = getAccessibility args.SyntaxArray args.NodeIndex
                getPatternIdents accessibility (getValueOrFunctionIdents args.CheckInfo) true pattern
            | _ -> Array.empty
        else
            Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "PublicValuesNames"
      Identifier = Identifiers.PublicValuesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule