exception Unbalanced
exception WrongType
exception ArgumentCount
exception NotFound
exception NotAFunction

infix |>;
fun (g |> f) = f g;

fun coalesce options =
    case options of
      nil => NONE
    | thunk :: rest =>
        case thunk () of
          NONE => coalesce rest
        | x    => x
