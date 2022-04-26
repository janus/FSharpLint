module FSharpLint.Rules.GenericTypesNames

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper.Naming
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework


let private generateError (identifier: Ident, message: string, suggestedFix) =
    let error =
        { Range = identifier.idRange
          Message = message
          SuggestedFix = Some suggestedFix
          TypeChecks = [] }

    let formatError errorName =
        String.Format(Resources.GetString errorName, message)

    let tryAddFix message = (identifier, message, QuickFixes.toPascalCase identifier)

    "RulesNamingConventionsPascalCaseErrorSpecificAdviceForGenericTypesSpecificTypeName"
    |> formatError |> tryAddFix |> Array.singleton
    |> Array.map error

let private isTargetSpec(text: string) =
    not (isPascalCase text)  &&  text = "a"

let private findSpecs(types: List<SynTyparDecl>) =
    let rec loop declares visited specs =
        match declares with
        | SynTyparDecl(_attr, SynTypar(id, _, _))::rest when isTargetSpec id.idText ->
            loop rest visited ((id, id.idText, None)::specs)
        | head::rest ->
            loop rest (head::visited) specs
        | [] -> List.rev visited, List.rev specs
    loop types [] []

let private getIdentifiers (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn(componentInfo, _typeDef, _, _, _)) ->
        let checkTypes types =
            seq {
                for SynTyparDecl(_attr, synTypeDecl) in types do
                    match synTypeDecl with
                    | SynTypar(id, _, _) when not (isPascalCase id.idText) ->
                        yield (id, id.idText, None)
                    | _ -> ()
            }
            
        match componentInfo with
        | SynComponentInfo(_attrs, types, _, _identifier, _, _, _, _) ->
            let (visited, specs) = findSpecs types
            if not List.isEmpty specs then
                specs |> Array.collect generateError
                checkTypes visited |> Array.ofSeq
            else
                checkTypes types |> Array.ofSeq
    | _ -> Array.empty

let rule config =
    { Name = "GenericTypesNames"
      Identifier = Identifiers.GenericTypesNames
      RuleConfig = { NamingRuleConfig.Config = config; GetIdentifiersToCheck = getIdentifiers } }
    |> toAstNodeRule
    |> AstNodeRule