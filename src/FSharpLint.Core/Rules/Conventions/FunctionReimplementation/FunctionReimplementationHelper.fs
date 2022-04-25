module FSharpLint.Rules.Helper.FunctionReimplementation

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let rec getLambdaParamIdent = function
    | SynSimplePats.SimplePats([pattern], _) -> 
        let rec getIdent = function
            | SynSimplePat.Id(ident, _, _, _, _, _) -> ident
            | SynSimplePat.Typed(simplePattern, _, _)
            | SynSimplePat.Attrib(simplePattern, _, _) ->
                getIdent simplePattern

        getIdent pattern |> Some
    | SynSimplePats.SimplePats(_) -> None
    | SynSimplePats.Typed(simplePatterns, _, _) -> 
        getLambdaParamIdent simplePatterns

let isTargetBinding bindings =
    let rec loop synBindings =
        match synBindings with
        | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(LongIdentWithDots(identifiers, _), _, _, _args, _, _), _, _, _, _) :: _ ->
            identifiers.[0].idText.StartsWith "|" && identifiers.[0].idText.EndsWith "|"
        | _  ->  false
    loop bindings

let isForActivePatten args =
    let rec loop index =
        let parents = List.rev (args.GetParents index)
        match parents with
        | AstNode.ModuleDeclaration(SynModuleDecl.Let (_, bindings, _)) :: _rest ->
            isTargetBinding bindings
        | AstNode.ModuleOrNamespace(_) :: rest ->
            match rest with
            | AstNode.ModuleDeclaration(SynModuleDecl.Let (_, bindings, _)) :: _rest ->
                isTargetBinding bindings
            | _ -> false
        | _ -> loop (index + 1)
    loop 1

let checkLambda (args:AstNodeRuleParams) checker =
    match args.AstNode with
    | AstNode.Expression(SynExpr.Lambda(_)) as lambda ->
        if isForActivePatten args then
            Array.empty
        else
            match lambda with
            | Lambda(lambda, range) ->
                if (not << List.isEmpty) lambda.Arguments then
                    checker args.FileContent lambda range
                else Array.empty
            | _ -> Array.empty
    | _ -> Array.empty
