type reader = string * int

fun read (s: string) = (s, 0)

fun peek reader = String.sub reader
fun next (s, i) = (s, i + 1)
fun rest (s, i) = String.extract (s, i, NONE)
fun toend (s, i) = (s, String.size s)
fun atend (s: string, i: int) = i >= (String.size s)

fun stringify lst = List.rev lst |> String.implode


fun isSpace c = Char.isSpace c orelse c = #","

fun skipSpace reader =
    if not (atend reader) andalso peek reader |> isSpace
    then next reader |> skipSpace
    else reader

fun readSpliceUnquote reader =
    if (peek reader = #"~") andalso (peek (next reader) = #"@")
    then SOME ("~@", next (next reader))
    else NONE

fun isSpecial c =
    case c of
      #"[" => true
    | #"{" => true
    | #"]" => true
    | #"}" => true
    | #"(" => true
    | #")" => true
    | #"'" => true
    | #"`" => true
    | #"~" => true
    | #"^" => true
    | #"@" => true
    | _    => false

fun readSpecialChar reader =
    if peek reader |> isSpecial
    then SOME (peek reader |> Char.toString, next reader)
    else NONE

fun readStringLiteral reader =
    let
        fun readToEnd reader escape acc =
            if atend reader
            then (stringify acc, reader)
            else
                case (peek reader, escape) of
                  (#"\"", false) => (#"\"" :: acc |> stringify, next reader)
                | (#"\"", true ) => readToEnd (next reader) false (#"\"" :: acc)
                | (#"\\", true ) => readToEnd (next reader) false (#"\\" :: acc)
                | (#"\\", false) => readToEnd (next reader) true (#"\\" :: acc)
                | (c    , _    ) => readToEnd (next reader) false (c :: acc) 
    in
        if peek reader = #"\""
        then SOME (readToEnd (next reader) false [#"\""])
        else NONE
    end

fun readComment reader =
    if peek reader = #";"
    then SOME (rest reader, toend reader)
    else NONE

fun readOtherToken reader =
    let
        fun isBoring c = (isSpecial c) orelse (isSpace c) orelse (c = #";")
        fun helper reader acc =
            if (atend reader) orelse (peek reader |> isBoring)
            then (stringify acc, reader)
            else helper (next reader) (peek reader :: acc)
    in
        if peek reader |> isBoring
        then NONE
        else SOME (helper (next reader) [peek reader])
    end

fun tokenize reader =
    let
        fun readAny reader =
            if atend reader 
            then NONE
            else coalesce [
                fn () => readSpliceUnquote reader,
                fn () => readSpecialChar reader,
                fn () => readStringLiteral reader,
                fn () => readComment reader,
                fn () => readOtherToken reader
            ]
        fun helper reader acc =
            case skipSpace reader |> readAny of
                NONE => List.rev acc
              | SOME (token, newReader) => helper newReader (token :: acc)
    in
        helper reader nil
    end

fun read_form tokens =
    let
        fun accumulate rbracket valtype tokens acc =
            case tokens of
                  nil => raise Unbalanced
                | token :: rest =>
                    if token = rbracket
                    then (valtype (List.rev acc), rest)
                    else
                        let
                            val (form, rest) = read_form tokens
                        in
                            accumulate rbracket valtype rest (form :: acc)
                        end

        fun read_list tokens = accumulate ")" MalList tokens nil

        fun read_vector tokens = accumulate "]" MalVector tokens nil

        val read_atom = toMalVal
    in
        case tokens of
              nil           => (Nil, nil)
            | "("   :: rest => read_list rest
            | "["   :: rest => read_vector rest
            | token :: rest => (read_atom token, rest) 
    end

fun read_str str =
    read str
    |> tokenize
    |> read_form
    |> #1
        
