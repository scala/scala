import scala.reflect.runtime.universe._

object Test {
  /** Collects symbols by the given name, even if they're not
   *  named CC.
   */
  def collectSymbols[T: TypeTag](inMethod: TermName, name: String): List[String] = {
    val m = typeOf[T] member inMethod typeSignatureIn typeOf[T]
    var buf: List[Symbol] = Nil
    var seen: Set[Symbol] = Set()
    def id(s: Symbol): Int = s.asInstanceOf[{ def id: Int }].id

    def check(s: Symbol) {
      if (!seen(s)) {
        seen += s
        if (s.name.toString == name) buf ::= s
      }
    }
    def loop(t: Type) {
      t match {
        case TypeRef(pre, sym, args)    => loop(pre) ; check(sym) ; args foreach loop
        case PolyType(tparams, restpe)  => tparams foreach { tp => check(tp) ; check(tp.owner) ; loop(tp.typeSignature) } ; loop(restpe)
        case MethodType(params, restpe) => params foreach { p => check(p) ; loop(p.typeSignature) } ; loop(restpe)
        case _                          =>
      }
    }
    loop(m)

    buf.reverse.distinct map (s => s.name + "#" + id(s))
  }

  def main(args: Array[String]): Unit = {
    val syms = collectSymbols[s.Bar]("to", "CC")
    assert(syms.size == 1, syms)
    println("OK!")
  }
}
