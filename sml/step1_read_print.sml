structure IO = TextIO

(* Defining locals mostly to avoid shadowing the real print statement. *)
local
    fun read s = read_str s
    fun eval l = l
    fun print l = pr_str l
in
    fun rep s = s |> read |> eval |> print
        handle Unbalanced => "unbalanced"
end

fun main () = 
    let
        val _ = print "\nuser> ";
        val line = IO.inputLine IO.stdIn   
    in
        case line of
            NONE          => print "exiting (EOF)\n"
          | SOME "\\q\n" => print "exiting (quitting)\n"
          | SOME line     => print (rep line) |> main
    end

val _ = main ()
