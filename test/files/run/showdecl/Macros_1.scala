import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    var messages = List[String]()
    def println(msg: String) = messages :+= msg

    import c.universe._
    def test(sym: Symbol): Unit = {
      println(s"uninitialized ${sym.name}: ${showDecl(sym)}")
      sym.info // NOTE: not fullyInitializeSymbol, so some parts may still be LazyTypes
      println(s"initialized ${sym.name}: ${showDecl(sym)}")
    }

    println("compile-time")
    test(c.mirror.staticClass("D"))
    test(c.mirror.staticClass("D").info.member(TermName("x")))
    test(c.mirror.staticClass("D").info.member(TermName("y")))
    test(c.mirror.staticClass("D").info.member(TermName("z")))
    test(c.mirror.staticClass("D").info.member(TermName("t")))
    test(c.mirror.staticClass("D").info.member(TypeName("W")))
    test(c.mirror.staticClass("D").info.member(TypeName("C")))
    test(c.mirror.staticClass("D").info.member(TermName("O")))

    q"..${messages.map(msg => q"println($msg)")}"
  }

  def foo: Any = macro impl
}