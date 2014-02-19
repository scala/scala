import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    var messages = List[String]()
    def println(msg: String) = messages :+= msg

    import c.universe._
    def test(tpe: Type): Unit = {
      val sym = tpe.typeSymbol
      println(s"uninitialized ${sym.name}: ${sym.pos.source.file.name}")
      internal.initialize(sym)
      println(s"initialized ${sym.name}: ${sym.pos.source.file.name}")
    }

    println("compile-time")
    test(typeOf[java.io.File])
    test(typeOf[scala.collection.BitSet])
    test(c.mirror.staticClass("C").toType)

    q"..${messages.map(msg => q"println($msg)")}"
  }

  def foo: Any = macro impl
}