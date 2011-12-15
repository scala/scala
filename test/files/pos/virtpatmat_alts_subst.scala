case class Foo(s: String) {
  def appliedType(tycon: Any) =
    tycon match {
        case Foo(sym @ ("NothingClass" | "AnyClass")) => println(sym)
    }
}
