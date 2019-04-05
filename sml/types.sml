datatype malval =
      Nil
    | MalBoolean of bool
    | MalInteger of int
    | MalString  of string
    | MalSymbol  of string
    | MalList    of malval list
    | MalVector  of malval list
    | MalMap     of (malval * malval) list
    | MalFunc    of malval list -> malval

fun toMalVal s =
    let
        fun toMalBoolean "true"  = SOME (MalBoolean true)
          | toMalBoolean "false" = SOME (MalBoolean false)
          | toMalBoolean _       = NONE

        val isDigit = Char.isDigit
        fun isNumeric s =
            case String.explode s of
                only :: nil   => isDigit only
              | first :: rest => (
                    (first = #"-" orelse isDigit first)
                    andalso (List.all isDigit rest)
                )
              | _ => false

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
                fn () => toMalBoolean s,
                fn () => toMalInteger s,
                fn () => toMalString s,
                fn () => SOME (MalSymbol s)
            ]
        )
    end
