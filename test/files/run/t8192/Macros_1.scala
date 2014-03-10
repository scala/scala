import scala.reflect.macros.whitebox._
import scala.language.experimental.macros
import java.io._

object Macros {
  def impl(c: Context) = {
    var messages = List[String]()
    def println(msg: String) = messages :+= msg

    import c.universe._
    def test(sym: ClassSymbol): Unit = {
      def fullyInitializeSymbol(sym: Symbol): Unit = {
        val internal = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
        internal.definitions.fullyInitializeSymbol(sym.asInstanceOf[internal.Symbol])
      }
      def defString(sym: Symbol): String = {
        val internal = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
        sym.asInstanceOf[internal.Symbol].defString
      }
      def showCtor(sym: Symbol): String = {
        fullyInitializeSymbol(sym)
        if (sym == NoSymbol) "NoSymbol"
        else s"${defString(sym)} => ${sym.asMethod.isPrimaryConstructor}"
      }
      sym.info
      println(sym.toString)
      println(s"primary constructor: ${showCtor(sym.primaryConstructor)}")
      val ctors = sym.info.members.filter(_.name == termNames.CONSTRUCTOR).map(sym => showCtor(sym))
      ctors.toList.sorted.foreach(println)
    }

    println("compile-time")
    // SI-8367 primaryConstructor for Java-defined classes is unstable, so I'm commenting this out
    // test(typeOf[File].typeSymbol.asClass)
    test(definitions.ScalaPackageClass)
    test(definitions.ListModule.moduleClass.asClass)
    test(typeOf[Product1[_]].typeSymbol.asClass)
    test(typeOf[UninitializedFieldError].typeSymbol.asClass)
    test(c.mirror.staticClass("C").asClass)

    q"..${messages.map(msg => q"println($msg)")}"
  }

  def foo: Any = macro impl
}