exception Unbalanced

infix |>;
fun (g |> f) = f g;

fun coalesce options =
    case options of
      nil => NONE
    | thunk :: rest =>
        case thunk () of
          NONE => coalesce rest
        | x    => x
