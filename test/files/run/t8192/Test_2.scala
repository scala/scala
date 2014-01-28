import java.io._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class C(x: Int) {
  def this(x: String) = this(x.toInt)
}

object Test extends App {
  def test(sym: ClassSymbol): Unit = {
    def fullyInitializeSymbol(sym: Symbol): Unit = {
      val internal = ru.asInstanceOf[scala.reflect.internal.SymbolTable]
      internal.definitions.fullyInitializeSymbol(sym.asInstanceOf[internal.Symbol])
    }
    def defString(sym: Symbol): String = {
      val internal = ru.asInstanceOf[scala.reflect.internal.SymbolTable]
      sym.asInstanceOf[internal.Symbol].defString
    }
    def showCtor(sym: Symbol): String = {
      fullyInitializeSymbol(sym)
      if (sym == NoSymbol) "NoSymbol"
      else s"${defString(sym)} => ${sym.asMethod.isPrimaryConstructor}"
    }
    sym.typeSignature
    println(sym.toString)
    println(s"primary constructor: ${showCtor(sym.primaryConstructor)}")
    val ctors = sym.typeSignature.members.filter(_.name == nme.CONSTRUCTOR).map(sym => showCtor(sym))
    ctors.toList.sorted.foreach(println)
  }

  Macros.foo
  println("runtime")
  test(typeOf[File].typeSymbol.asClass)
  test(definitions.ScalaPackageClass)
  test(definitions.ListModule.moduleClass.asClass)
  test(typeOf[Product1[_]].typeSymbol.asClass)
  test(typeOf[UninitializedFieldError].typeSymbol.asClass)
  test(typeOf[C].typeSymbol.asClass)
}
