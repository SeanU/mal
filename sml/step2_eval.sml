val inputLine = TextIO.inputLine
val stdIn = TextIO.stdIn
val lookup = Environment.lookup

(* Defining locals mostly to avoid shadowing the real print statement. *)
val READ = read_str

fun EVAL env ast =
    let
        fun eval_ast (MalSymbol s) = lookup env s
          | eval_ast (MalList l) = MalList (List.map (EVAL env) l)
          | eval_ast ast = ast

        fun eval_list l =
            case eval_ast l of 
                MalList ((MalFunc oper) :: args) => oper args
    in
        case ast of
            MalList nil => ast
          | MalList _   => eval_list ast
          | mv          => eval_ast mv
    end

val PRINT = pr_str

fun rep s = 
    let 
        val ast = READ s
        val result = EVAL Environment.default ast
    in
        PRINT result
    end
    handle Unbalanced => "unbalanced"
         | NotFound => "not found"

fun main () = 
    let
        val _ = print "\nuser> ";
        val line = inputLine stdIn   
    in
        case line of
            NONE          => print "exiting (EOF)\n"
          | SOME "\\q\n" => print "exiting (quitting)\n"
          | SOME line     => print (rep line) |> main
    end

val _ = main ()
