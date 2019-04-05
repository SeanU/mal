fun pr_str mv =
    let 
        fun pr_lst l lbrace rbrace =
            let
                val contents = 
                    l
                    |> List.map pr_str
                    |> String.concatWith " "
            in
                lbrace ^ contents ^ rbrace
            end

        fun flatten l =
            let
                fun helper ((k, v) :: l) acc = helper l (v :: k :: acc)
                  | helper nil acc = List.rev acc
            in
                helper l nil
            end 
    in
        case mv of
            Nil            => "Nil"
        | MalBoolean true  => "true"
        | MalBoolean false => "false"
        | MalInteger i     => 
                if i >= 0 
                then Int.toString i 
                else "-" ^ (Int.toString (i * ~1)) 
        | MalString s      => "\"" ^ s ^ "\""
        | MalSymbol s      => s
        | MalList l        => pr_lst l "(" ")"
        | MalVector l      => pr_lst l "[" "]"
        | MalMap l         => pr_lst (flatten l) "{" "}"
        | MalFunc f        => "FUNCTION"
    end