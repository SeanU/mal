structure IO = TextIO

(* Defining locals mostly to avoid shadowing the real print statement. *)
local
    fun read s = s
    fun eval s = s
    fun print s = s
in
    fun rep s = s |> read |> eval |> print
end

fun main () = 
    let
        val _ = print "\nuser> ";
        val line = IO.inputLine IO.stdIn   
    in
        case line of
            NONE      => print "exiting (EOF)\n"
          | SOME line => print (rep line) |> main
    end

val _ = main ()
