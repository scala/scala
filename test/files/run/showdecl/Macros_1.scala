import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    var messages = List[String]()
    def println(msg: String) = messages :+= msg

    import c.universe._
    def test(sym: Symbol): Unit = {
      println(s"uninitialized ${sym.name}: ${showDeclaration(sym)}")
      sym.typeSignature
      println(s"initialized ${sym.name}: ${showDeclaration(sym)}")
    }

    println("compile-time")
    test(c.mirror.staticClass("D"))
    test(c.mirror.staticClass("D").typeSignature.member(TermName("x")))
    test(c.mirror.staticClass("D").typeSignature.member(TermName("y")))
    test(c.mirror.staticClass("D").typeSignature.member(TermName("z")))
    test(c.mirror.staticClass("D").typeSignature.member(TermName("t")))
    test(c.mirror.staticClass("D").typeSignature.member(TypeName("W")))
    test(c.mirror.staticClass("D").typeSignature.member(TypeName("C")))
    test(c.mirror.staticClass("D").typeSignature.member(TermName("O")))

    q"..${messages.map(msg => q"println($msg)")}"
  }

  def foo: Any = macro impl
}