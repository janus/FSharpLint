module FSharpLint.Rules.Identifiers

let [<Literal>] private Prefix = "FL"

let identifier (number:int) =
    sprintf "%s%04d" Prefix number

let TupleCommaSpacing = identifier 1
let TupleIndentation = identifier 2
let TupleParentheses = identifier 3
let PatternMatchClausesOnNewLine = identifier 4
let PatternMatchOrClausesOnNewLine = identifier 5
let PatternMatchClauseIndentation = identifier 6
let PatternMatchExpressionIndentation = identifier 7
let ModuleDeclSpacing = identifier 8
let ClassMemberSpacing = identifier 9
let TypedItemSpacing = identifier 10
let TypePrefixing = identifier 11
let UnionDefinitionIndentation = identifier 12
let RecursiveAsyncFunction = identifier 13
let RedundantNewKeyword = identifier 14
let NestedStatements = identifier 15
let FailwithWithSingleArgument = identifier 16
let RaiseWithSingleArgument = identifier 17
let NullArgWithSingleArgument = identifier 18
let InvalidOpWithSingleArgument = identifier 19
let InvalidArgWithTwoArguments = identifier 20
let FailwithfWithArgumentsMatchingFormattingString = identifier 21
let MaxLinesInLambdaFunction = identifier 22
let MaxLinesInMatchLambdaFunction = identifier 23
let MaxLinesInValue = identifier 24
let MaxLinesInFunction = identifier 25
let MaxLinesInMember = identifier 26
let MaxLinesInConstructor = identifier 27
let MaxLinesInProperty = identifier 28
let MaxLinesInModule = identifier 29
let MaxLinesInRecord = identifier 30
let MaxLinesInEnum = identifier 31
let MaxLinesInUnion = identifier 32
let MaxLinesInClass = identifier 33
let ReimplementsFunction = identifier 34
let CanBeReplacedWithComposition = identifier 35
let InterfaceNames = identifier 36
let ExceptionNames = identifier 37
let TypeNames = identifier 38
let RecordFieldNames = identifier 39
let EnumCasesNames = identifier 40
let UnionCasesNames = identifier 41
let ModuleNames = identifier 42
let LiteralNames = identifier 43
let NamespaceNames = identifier 44
let MemberNames = identifier 45
let ParameterNames = identifier 46
let MeasureTypeNames = identifier 47
let ActivePatternNames = identifier 48
let PublicValuesNames = identifier 49
// let NonPublicValuesNames = identifier 50 - rule removed.
let MaxNumberOfItemsInTuple = identifier 51
let MaxNumberOfFunctionParameters = identifier 52
let MaxNumberOfMembers = identifier 53
let MaxNumberOfBooleanOperatorsInCondition = identifier 54
let FavourIgnoreOverLetWild = identifier 55
let WildcardNamedWithAsPattern = identifier 56
let UselessBinding = identifier 57
let TupleOfWildcards = identifier 58
let Indentation = identifier 59
let MaxCharactersOnLine = identifier 60
let TrailingWhitespaceOnLine = identifier 61
let MaxLinesInFile = identifier 62
let TrailingNewLineInFile = identifier 63
let NoTabCharacters = identifier 64
let Hints = identifier 65
let NoPartialFunctions = identifier 66
let PrivateValuesNames = identifier 67
let InternalValuesNames = identifier 68
let GenericTypesNames = identifier 69
let FavourTypedIgnore = identifier 70
let CyclomaticComplexity = identifier 71
let FailwithBadUsage = identifier 72
let FavourReRaise = identifier 73
let FavourConsistentThis = identifier 74
let AvoidTypeHintSuffixesForNames = identifier 75
