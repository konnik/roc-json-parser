##
## Simple JSON parsing into an ADT
##
module [
    Value,
    parseUtf8,
    parseString,
]

import unicode.CodePoint

Value : [
    True,
    False,
    Null,
    Number F64,
    String Str,
    Array (List Value),
    Object (Dict Str Value),
]

ParseError : [
    Fail Str,
]

parseUtf8 : List U8 -> Result Value ParseError
parseUtf8 = \input ->
    when CodePoint.parseUtf8 input is
        Ok codepoints -> parse codepoints
        Err _ -> error "Not valid UTF-8"

parseString : Str -> Result Value ParseError
parseString = \input ->
    input |> Str.toUtf8 |> parseUtf8
#
# IMPLEMENTATION
#

error : Str -> Result _ ParseError
error = \msg -> Err (Fail msg)

parse : List CodePoint.CodePoint -> Result Value ParseError
parse = \input ->
    when runParser json input is
        Ok (value, []) -> Ok value
        Ok (_, [..]) -> error "Not all input was consumed"
        Err err -> Err err

#
# JSON PARSER
#
#

json : Parser Value
json = element

jsvalue : Parser Value
jsvalue =
    oneOf [
        object |> map (\x -> Object x),
        array |> map (\x -> Array x),
        string |> map (\x -> String x),
        number |> map (\x -> Number x),
        true |> map (\x -> True),
        false |> map (\x -> False),
        null |> map (\x -> Null),
    ]

object : Parser (Dict Str Value)
object = surroundedBy '{' members '}' |> map \x -> Dict.fromList x

members : Parser (List (Str, Value))
members = optional whitespace |> keepRight (delimitedBy ',' member)

member : Parser (Str, Value)
member =
    key = whitespace |> keepRight string |> keepLeft whitespace |> keepLeft (char (toChar ':'))
    map2 (\k, e -> (k, e)) key element

array : Parser (List Value)
array = surroundedBy '[' elements ']'

elements : Parser (List Value)
elements = optional whitespace |> keepRight (delimitedBy ',' element)

element : Parser Value
element = whitespace |> keepRight element |> keepLeft whitespace

string : Parser Str
string =
    surroundedBy '"' characters '"'
    |> andThen \chars ->
        when CodePoint.toStr chars is
            Ok strVal -> succeed strVal
            Err _ -> fail "Could not convert characters to string."

characters : Parser (List Char)
characters = many character

character : Parser Char
character =
    x = Set.fromList ['"', '\\']
    oneOf [
        match \ch -> !(Set.contains x (CodePoint.toU32 ch)),
        char (toChar '\\') |> andThen \_ -> escape,
    ]
escape : Parser Char
escape =
    mapChar = \from, to -> char (toChar from) |> keepRight (succeed (toChar to))

    oneOf [
        mapChar '"' '"',
        mapChar '\\' '\\',
        mapChar '/' '/',
        mapChar 'b' 0x08,
        mapChar 'f' 0x0c,
        mapChar 'n' '\n',
        mapChar 'r' '\r',
        mapChar 't' '\t',
        char (toChar 'u') |> andThen \_ -> combine [hex, hex, hex, hex] |> map hexToChar,
    ]

hexToChar : List U32 -> Char
hexToChar = \chars ->
    when chars is
        [a, b, c, d] -> toChar (a * 4096 + b * 256 + c * 16 + d)
        _ -> crash "BUG: not 4 hex characters"

hex : Parser U32
hex =
    next
    |> andThen \ch ->
        when CodePoint.toU32 ch is
            '0' -> succeed 0
            '1' -> succeed 1
            '2' -> succeed 2
            '3' -> succeed 3
            '4' -> succeed 4
            '5' -> succeed 5
            '6' -> succeed 6
            '7' -> succeed 7
            '8' -> succeed 8
            '9' -> succeed 9
            'A' -> succeed 10
            'B' -> succeed 11
            'C' -> succeed 12
            'D' -> succeed 13
            'E' -> succeed 14
            'F' -> succeed 15
            'a' -> succeed 10
            'b' -> succeed 11
            'c' -> succeed 12
            'd' -> succeed 13
            'e' -> succeed 14
            'f' -> succeed 15
            _ -> fail "Not a valid hex char: $(charToStr ch)"

number : Parser F64
number =
    integer
    |> andThen \i ->
        fraction
        |> andThen \f ->
            exponent
            |> andThen \e ->
                when List.join [i, f, e] |> CodePoint.toStr is
                    Ok numStr ->
                        when Str.toF64 numStr is
                            Ok num -> succeed num
                            Err _ -> fail "BUG: could not convert number $(numStr) to F64"

                    Err _ -> fail "BUG: could not convert code points to number string"

integer : Parser (List Char)
integer =
    oneOf [
        char (toChar '-') |> cons (onenine |> cons digits),
        char (toChar '-') |> cons (onenine |> cons (succeed [])),
        onenine |> cons digits,
        digit |> cons (succeed []),
    ]

fraction : Parser (List Char)
fraction =
    oneOf [
        char (toChar '.') |> cons digits,
        succeed [],
    ]

exponent : Parser (List Char)
exponent = oneOf [
    join [str "E", sign, digits],
    join [str "e", sign, digits],
    succeed [],
]

digit : Parser Char
digit = oneOf [
    char (toChar '0'),
    onenine,
]

onenine : Parser Char
onenine =
    oneToNine = Set.fromList ['1', '2', '3', '4', '5', '6', '7', '8', '9']
    match \ch ->
        Set.contains oneToNine (CodePoint.toU32 ch)

digits : Parser (List Char)
digits =
    digit |> cons digits # måste köra lazy här?

sign : Parser (List Char)
sign = oneOf [
    str "+",
    str "-",
    succeed [],
]

true : Parser Bool
true = str "true" |> map \_ -> Bool.true

false : Parser Bool
false = str "false" |> map \_ -> Bool.false

null : Parser {}
null = str "null" |> map \_ -> {}

whitespace : Parser {}
whitespace =
    wsChars = Set.fromList [0x0020, 0x000A, 0x000D, 0x0009]
    many
        (
            match \ch ->
                Set.contains wsChars (CodePoint.toU32 ch)
        )
    |> map (\_ -> {})

#
# LOW LEVEL PARSER STUFF
#

# Lets base our parsing on a stream of unicode code points.
Char : CodePoint.CodePoint

# Classic definition of a recursive descent parser.
Parser a := List Char -> Result (a, List Char) ParseError

runParser : Parser a, List Char -> Result (a, List Char) ParseError
runParser = \@Parser p, input -> p input

# Primitive parser combinators

succeed : a -> Parser a
succeed = \value -> @Parser \input ->
        Ok (value, input)

fail : Str -> Parser a
fail = \msg -> @Parser \input ->
        error msg

next : Parser Char
next = @Parser \input ->
    when input is
        [a, .. as rest] -> Ok (a, rest)
        [] -> error "Expected at least one more character but the input was empty"

char : Char -> Parser Char
char = \ch -> @Parser \input ->
        when input is
            [a, .. as rest] ->
                if charsEqual a ch then
                    Ok (a, rest)
                else
                    error "Expected charachter '$(charToStr ch)' but got '$(charToStr a)'"

            [] -> error "Expected at least one more character but the input was empty"

match : (Char -> Bool) -> Parser Char
match = \predicate -> @Parser \input ->
        x = runParser next input
        when x is
            Ok (ch, rest) ->
                if predicate ch then
                    Ok (ch, rest)
                else
                    error "The char '$(charToStr ch)' does not match the predicate."

            Err err -> Err err

str : Str -> Parser (List Char)
str = \value -> @Parser \input ->
        when CodePoint.parseUtf8 (Str.toUtf8 value) is
            Ok codepoints ->
                x =
                    codepoints
                    |> List.map char
                    |> combine

                runParser x input

            Err _ -> error "The string '$(value)' could not be parsed as unicode codepoints."

#
# Convenience functions
#

charsEqual : Char, Char -> Bool
charsEqual = \ch1, ch2 -> CodePoint.toU32 ch1 == CodePoint.toU32 ch2

charToStr : Char -> Str
charToStr = \ch -> [ch] |> CodePoint.toStr |> Result.withDefault "<???>"

toChar : U32 -> Char
toChar = \n ->
    when CodePoint.fromU32 n is
        Ok ch -> ch
        Err _ -> crash "Invalid code point: $(Num.toStr n)"

#
# Combinators
#

map : Parser a, (a -> b) -> Parser b
map = \parserA, aToB -> @Parser \input ->
        when runParser parserA input is
            Ok (a, rest) -> Ok (aToB a, rest)
            Err err -> Err err

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = \parserA, aToParserB -> @Parser \input ->
        when runParser parserA input is
            Ok (a, rest) -> runParser (aToParserB a) rest
            Err err -> Err err

tryMap : Parser a, (a -> Result b _), Str -> Parser b
tryMap = \parserA, aToResultB, errorMsg ->
    parserA
    |> andThen \a ->
        when aToResultB a is
            Ok b -> succeed b
            Err _ -> fail errorMsg

combine : List (Parser a) -> Parser (List a)
combine = \parsers -> @Parser \input ->
        when parsers is
            [] -> Ok ([], input)
            [parserA, .. as restOfParsers] ->
                when runParser parserA input is
                    Err err -> Err err
                    Ok (a, rest) ->
                        when runParser (combine restOfParsers) rest is
                            Err err -> Err err
                            Ok (aList, rest2) -> Ok (List.prepend aList a, rest2)

oneOf : List (Parser a) -> Parser a
oneOf = \choices -> @Parser \input ->
        when choices is
            [] -> error "No choice matched."
            [p, .. as moreParsers] ->
                when runParser p input is
                    Ok (a, rest) -> Ok (a, rest)
                    Err _ -> runParser (oneOf moreParsers) input

delimitedBy : U32, Parser a -> Parser (List a)
delimitedBy = \delimiter, parserA ->
    oneOf [
        cons parserA (many (keepRight (char (toChar delimiter)) parserA)),
        succeed [],
    ]

surroundedBy : U32, Parser a, U32 -> Parser a
surroundedBy = \left, parserA, right ->
    keepRight (char (toChar left)) (keepLeft parserA (char (toChar right)))

keepRight : Parser a, Parser b -> Parser b
keepRight = \parserA, parserB ->
    map2 (\a, b -> b) parserA parserB

keepLeft : Parser a, Parser b -> Parser a
keepLeft = \parserA, parserB ->
    map2 (\a, b -> a) parserA parserB

map2 : (a, b -> c), Parser a, Parser b -> Parser c
map2 = \f, parserA, parserB ->
    parserA
    |> andThen \a ->
        parserB
        |> andThen \b ->
            succeed (f a b)

cons : Parser a, Parser (List a) -> Parser (List a)
cons = \parserA, parserB ->
    map2 List.prepend parserB parserA

many : Parser a -> Parser (List a)
many = \parserA -> @Parser \input ->
        when runParser parserA input is
            Ok (a, rest) ->
                when runParser (many parserA) rest is
                    Ok (moreAs, rest2) -> Ok (List.prepend moreAs a, rest2)
                    Err err -> Err err

            Err _ -> Ok ([], input)

Option a : [
    Some a,
    None,
]

join : List (Parser (List a)) -> Parser (List a)
join = \parsers ->
    parsers |> combine |> map List.join

optional : Parser a -> Parser (Option a)
optional = \parserA ->
    oneOf [
        parserA |> map (\x -> Some x),
        succeed None,
    ]
