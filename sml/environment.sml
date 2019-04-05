structure Environment = struct
    type entry = string * malval
    type environment = entry list

    fun lookup (env: environment) (key: string) =
        let
            fun helper e =
                case e of
                      nil => raise NotFound
                    | (k, v) :: rest =>
                        if k = key
                        then v
                        else helper rest 
        in
            helper env
        end

    fun toint (MalInteger i) = i
      | toint _ = raise WrongType

    fun reduce opr ls =
        let
            fun helper (hd :: tl) acc = helper tl (opr (acc, hd))
              | helper nil acc = acc
        in
            case ls of
                  hd :: tl => helper tl hd
                | nil => raise ArgumentCount
        end

    fun intfunc opr ls = ls |> List.map toint |> reduce opr |> MalInteger

    val default: environment = [
        ("+", MalFunc (intfunc (op +))),
        ("-", MalFunc (intfunc (op -))),
        ("*", MalFunc (intfunc (op *))),
        ("/", MalFunc (intfunc (op div)))
    ]
end
