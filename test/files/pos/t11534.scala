//> using options -Werror
object Test1 {
    val g: scala.tools.nsc.Global = ???
    import g._
    def test(sym: Symbol) = sym.name match {
        case _: TermName  =>
    }
}
