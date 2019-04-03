fun load () = (
    use "util.sml";
    use "types.sml";
    use "reader.sml";
    use "printer.sml"
)

fun run () = (
    load ();
    use "step1_read_print.sml"
)
