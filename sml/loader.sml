fun load () = (
    use "util.sml";
    use "types.sml";
    use "environment.sml";
    use "reader.sml";
    use "printer.sml"
)

fun run () = (
    load ();
    use "step2_eval.sml"
)
