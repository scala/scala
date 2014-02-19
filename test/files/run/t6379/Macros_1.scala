import scala.reflect.macros.whitebox._
import scala.language.experimental.macros
import java.io._

object Macros {
  def impl(c: Context) = {
    var messages = List[String]()
    def println(msg: String) = messages :+= msg

    import c.universe._
    def test(sym: MethodSymbol): Unit = {
      println(s"uninitialized ${sym.name}: ${sym.exceptions}")
      sym.info
      println(s"initialized ${sym.name}: ${sym.exceptions}")
    }

    println("compile-time")
    test(typeOf[Closeable].declaration(TermName("close")).asMethod)
    test(typeOf[Product1[_]].declaration(TermName("productElement")).asMethod)
    test(c.mirror.staticClass("Reader").info.decl(TermName("read")).asMethod)

    q"..${messages.map(msg => q"println($msg)")}"
  }

  def foo: Any = macro impl
}