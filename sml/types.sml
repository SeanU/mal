datatype malval =
      Nil
    | MalInteger of int
    | MalString  of string
    | MalSymbol  of string
    | MalList    of malval list
    | MalVector  of malval list

fun toMalVal s =
    let
        fun isNumeric s = List.all Char.isDigit (String.explode s) 
        fun toMalInteger s =
            if isNumeric s
            then SOME (MalInteger (Option.valOf (Int.fromString s)))
            else NONE

        fun isString s = String.isPrefix "\"" s
        fun stripQuotes s = 
            if String.isSuffix "\"" s
            then String.substring(s, 1, String.size s - 2)
            else raise Unbalanced        
        fun toMalString s =
            if isString s
            then SOME (MalString (stripQuotes s))
            else NONE
    in
        Option.valOf (
            coalesce [
                fn () => toMalInteger s,
                fn () => toMalString s,
                fn () => SOME (MalSymbol s)
            ]
        )
    end
