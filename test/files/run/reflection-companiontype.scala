import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class C
object C

object Test extends App {
  type T = C

  println("TypeRefs")
  println(showRaw(typeOf[C].companion, printKinds = true))
  println(showRaw(typeOf[C].companion.companion, printKinds = true))
  println(showRaw(typeOf[C.type].companion, printKinds = true))
  println("ClassInfoTypes")
  println(showRaw(typeOf[C].typeSymbol.info.companion, printKinds = true))
  println(showRaw(typeOf[C].typeSymbol.info.companion.typeSymbol.info.companion, printKinds = true))
  println(showRaw(typeOf[C.type].typeSymbol.info.companion, printKinds = true))
  println("Unrelated")
  println(showRaw(typeOf[T].companion, printKinds = true))
  println(showRaw(cm.staticPackage("scala").moduleClass.asType.toType.companion, printKinds = true))
  println(showRaw(cm.staticPackage("scala").info.companion, printKinds = true))
}